use std::{cell::RefCell, fmt};

use rustc_hash::FxHashSet;

// === Rust patterns === //

pub trait ExtensionFor<T: ?Sized> {}

impl<T: ?Sized> ExtensionFor<T> for T {}

// === Formatters === //

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
