use std::{
    cell::RefCell,
    fmt,
    mem::{size_of_val, ManuallyDrop},
    rc::{Rc, Weak},
};

use rustc_hash::FxHashSet;

// === Formatting === //

pub struct DebugUsingDisplay<'a, T: ?Sized>(pub &'a T);

impl<T: ?Sized + fmt::Display> fmt::Debug for DebugUsingDisplay<'_, T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.0.fmt(f)
    }
}

pub struct FmtNoCycle<'a, T: ?Sized>(pub &'a T);

impl<T: ?Sized + fmt::Debug> fmt::Debug for FmtNoCycle<'_, T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        thread_local! {
            static FMT_CYCLES: RefCell<FxHashSet<usize>> = RefCell::new(FxHashSet::default());
        }

        FMT_CYCLES.with(|cycles| {
            let addr = self.0 as *const T as *const () as usize;
            let is_cycle = {
                let mut cycles = cycles.borrow_mut();
                cycles.len() > 100 || !cycles.insert(addr)
            };

            if is_cycle {
                write!(f, "<cyclic ref to {:?}>", addr as *const ())
            } else {
                struct Guard(usize);

                impl Drop for Guard {
                    fn drop(&mut self) {
                        FMT_CYCLES.with(|cycles| cycles.borrow_mut().remove(&self.0));
                    }
                }

                let _guard = Guard(addr);
                self.0.fmt(f)
            }
        })
    }
}

// === RC Helpers === //

// Weak coercions
fn addr_of<T: ?Sized>(p: *const T) -> usize {
    p as *const () as usize
}

pub fn coerce_weak<T: ?Sized, V: ?Sized>(rc: Weak<T>, f: impl FnOnce(&T) -> &V) -> Weak<V> {
    assert_ne!(rc.strong_count(), 0);

    let old_rc = Weak::into_raw(rc);
    let old_size = size_of_val::<T>(unsafe {
        // Safety: `strong_count() > 0` and `Weak<T>` will not be dropped while the function runs.
        &*old_rc
    });

    let new_rc = f(unsafe {
        // Safety: ibid
        &*old_rc
    });
    let new_size = size_of_val::<V>(new_rc);

    assert_eq!(addr_of(old_rc), addr_of(new_rc));
    assert_eq!(old_size, new_size);

    unsafe {
        // Safety: the output has the same size and address and `f` proves that the transmutation is
        // valid for all lifetimes
        Weak::from_raw(new_rc)
    }
}

// TransRc
#[repr(transparent)]
pub struct TransRc<T: ?Sized> {
    // This secretly represents both a `Weak<T>` and an `Rc<T>`.
    weak: Weak<T>,
}

impl<T: ?Sized> From<Rc<T>> for TransRc<T> {
    fn from(value: Rc<T>) -> Self {
        Self {
            // Construct a new `Weak` while leaking the `Rc`.
            weak: Rc::downgrade(&ManuallyDrop::new(value)),
        }
    }
}

impl<T: ?Sized> Clone for TransRc<T> {
    fn clone(&self) -> Self {
        unsafe {
            // Ensure that each `Weak` has a corresponding `Rc`.
            Rc::increment_strong_count(self.weak.as_ptr());

            Self {
                weak: self.weak.clone(),
            }
        }
    }
}

impl<T: ?Sized> TransRc<T> {
    pub fn coerce<V: ?Sized>(self, f: impl FnOnce(&T) -> &V) -> TransRc<V> {
        let weak = unsafe {
            // Safety: `self` is `Sized` and repr(transparent) w.r.t `Weak<T>`
            std::mem::transmute::<TransRc<T>, Weak<T>>(self)
        };

        TransRc {
            // This doesn't change the underlying shape of the `Weak` beyond potentially changing its
            // pointer metadata so we don't have to recreate the `Rc`.
            weak: coerce_weak(weak, f),
        }
    }

    pub fn as_weak(&self) -> &Weak<T> {
        &self.weak
    }

    pub fn get(&self) -> &T {
        unsafe {
            // Safety: our leaked `Rc` keeps the value alive
            &*Weak::as_ptr(&self.weak)
        }
    }
}

impl<T: ?Sized> Drop for TransRc<T> {
    fn drop(&mut self) {
        drop(unsafe {
            // Safety: by invariant, each `Weak` has a corresponding `Rc`
            Rc::from_raw(self.weak.as_ptr())
        });
    }
}
