use std::ops::{Deref, DerefMut};

pub trait LeaseProvider: Fn() -> Self::Provided {
    type Provided;
}

impl<O, T: ?Sized + Fn() -> O> LeaseProvider for T {
    type Provided = O;
}

#[non_exhaustive]
pub struct Leased<P: LeaseProvider> {
    #[doc(hidden)]
    pub __leased_ref_guard: P::Provided,
    #[doc(hidden)]
    pub __leased_ref_provider: P,
}

impl<G, P: Fn() -> G> Leased<P> {
    pub fn new(provider: P) -> Self {
        Self {
            __leased_ref_guard: provider(),
            __leased_ref_provider: provider,
        }
    }
}

impl<P: LeaseProvider> Deref for Leased<P>
where
    P::Provided: Deref,
{
    type Target = <P::Provided as Deref>::Target;

    fn deref(&self) -> &Self::Target {
        &self.__leased_ref_guard
    }
}

impl<P: LeaseProvider> DerefMut for Leased<P>
where
    P::Provided: DerefMut,
{
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.__leased_ref_guard
    }
}

#[doc(hidden)]
pub mod macro_internals {
    pub use std::mem::drop;
}

#[macro_export]
macro_rules! lease {
    (acquire $($name:ident)*) => {
        $($name.__leased_ref_guard = ($name.__leased_ref_provider)();)*
    };
    (release $($name:ident)*) => {
        $($crate::lease::macro_internals::drop($name.__leased_ref_guard);)*
    };
    ($($name:ident)*: $expr:expr) => {{
        $($crate::lease::macro_internals::drop($name.__leased_ref_guard);)*
        let res = $expr;
        $($name.__leased_ref_guard = ($name.__leased_ref_provider)();)*
        res
    }};
    ($($name:ident)*: $($tt:tt)*) => {
        $($crate::lease::macro_internals::drop($name.__leased_ref_guard);)*
        $($tt)*
        $($name.__leased_ref_guard = ($name.__leased_ref_provider)();)*
    };
}

pub use lease;
