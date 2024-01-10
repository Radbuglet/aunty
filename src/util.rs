use std::{
    cell::RefCell,
    fmt,
    mem::size_of_val,
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

// === Coercions === //

fn addr_of<T: ?Sized>(p: *const T) -> usize {
    p as *const () as usize
}

pub fn coerce_rc<T: ?Sized, V: ?Sized>(rc: Rc<T>, f: impl FnOnce(&T) -> &V) -> Rc<V> {
    let old_size = size_of_val::<T>(&*rc);
    let old_rc = Rc::into_raw(rc);

    let new_rc = f(unsafe { &*old_rc });
    let new_size = size_of_val::<V>(new_rc);

    assert_eq!(addr_of(old_rc), addr_of(new_rc));
    assert_eq!(old_size, new_size);

    unsafe { Rc::from_raw(new_rc) }
}

pub fn coerce_weak<T: ?Sized, V: ?Sized>(rc: Weak<T>, f: impl FnOnce(&T) -> &V) -> Weak<V> {
    assert_ne!(rc.strong_count(), 0);

    let old_rc = Weak::into_raw(rc);
    let old_size = size_of_val::<T>(unsafe { &*old_rc });

    let new_rc = f(unsafe { &*old_rc });
    let new_size = size_of_val::<V>(new_rc);

    assert_eq!(addr_of(old_rc), addr_of(new_rc));
    assert_eq!(old_size, new_size);

    unsafe { Weak::from_raw(new_rc) }
}
