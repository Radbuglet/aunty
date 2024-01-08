use std::fmt;

pub trait Delegate: fmt::Debug + Clone {}

#[doc(hidden)]
pub mod delegate_macro_internal {
    use std::mem::MaybeUninit;

    pub use {
        super::Delegate,
        std::{
            clone::Clone,
            convert::From,
            fmt,
            marker::PhantomData,
            ops::{Deref, Fn},
            panic::Location,
            rc::Rc,
            stringify,
        },
    };

    // N.B. this function is not marked as unsafe because `#[forbid(dead_code)]` may be used in
    // userland crates.
    #[allow(unsafe_code)] // TODO: Move to `core`
    pub fn uber_dangerous_transmute_this_is_unsound<A, B>(a: A) -> B {
        unsafe {
            let mut a = MaybeUninit::<A>::new(a);
            a.as_mut_ptr().cast::<B>().read()
        }
    }
}

#[macro_export]
macro_rules! delegate {
    (
        $(#[$attr_meta:meta])*
        $vis:vis fn $name:ident
            $(
                <$($generic:ident),* $(,)?>
                $(<$($fn_lt:lifetime),* $(,)?>)?
            )?
            ($($para_name:ident: $para:ty),* $(,)?) $(-> $ret:ty)?
        $(as deriving $deriving:path $({ $($deriving_args:tt)* })? )*
        $(where $($where_token:tt)*)?
    ) => {
        $(#[$attr_meta])*
        $vis struct $name <
            $($($generic,)*)?
            Marker = (),
            Handler: ?Sized =
                $($(for<$($fn_lt),*>)?)?
                dyn $crate::delegate::delegate_macro_internal::Fn(
                    $crate::delegate::delegate_macro_internal::PhantomData<$name<$($($generic,)*)? Marker, ()>>
                    $(,$para)*
                ) $(-> $ret)?,
        >
        $(where
            $($where_token)*
        )? {
            _ty: (
                $crate::delegate::delegate_macro_internal::PhantomData<fn() -> Marker>,
                $($($crate::delegate::delegate_macro_internal::PhantomData<fn() -> $generic>,)*)?
            ),
            #[cfg(debug_assertions)]
            defined: &'static $crate::delegate::delegate_macro_internal::Location<'static>,
            handler: $crate::delegate::delegate_macro_internal::Rc<Handler>,
        }

        #[allow(unused)]
        impl<$($($generic),*)?> $name<$($($generic,)*)?>
        $(where
            $($where_token)*
        )? {
            #[cfg_attr(debug_assertions, track_caller)]
            pub fn new<Func>(handler: Func) -> Self
            where
                Func: 'static +
                    $($(for<$($fn_lt),*>)?)?
                        Fn($($para),*) $(-> $ret)?,
            {
                Self::new_raw($crate::delegate::delegate_macro_internal::Rc::new(
                    move |_marker $(,$para_name)*| handler($($para_name),*)
                ))
            }
        }

        #[allow(unused)]
        impl<
            $($($generic,)*)?
            Marker,
            Handler: ?Sized +
                $($(for<$($fn_lt),*>)?)?
                $crate::delegate::delegate_macro_internal::Fn(
                    $crate::delegate::delegate_macro_internal::PhantomData<$name<$($($generic,)*)? Marker, ()>>
                    $(,$para)*
                ) $(-> $ret)?,
        > $name <$($($generic,)*)? Marker, Handler>
        $(where
            $($where_token)*
        )? {
            #[cfg_attr(debug_assertions, track_caller)]
            pub fn new_raw(handler: $crate::delegate::delegate_macro_internal::Rc<Handler>) -> Self {
                Self {
                    _ty: (
                        $crate::delegate::delegate_macro_internal::PhantomData::<fn() -> Marker>,
                        $($($crate::delegate::delegate_macro_internal::PhantomData::<fn() -> $generic>,)*)?
                    ),
                    #[cfg(debug_assertions)]
                    defined: $crate::delegate::delegate_macro_internal::Location::caller(),
                    handler,
                }
            }

            #[allow(non_camel_case_types)]
            pub fn call<$($($($fn_lt,)*)?)? $($para_name,)* __Out>(&self $(,$para_name: $para_name)*) -> __Out
            where
                $($(for<$($fn_lt,)*>)?)? fn($($para,)*) $(-> $ret)?: $crate::delegate::delegate_macro_internal::Fn($($para_name,)*) -> __Out,
            {
                $crate::delegate::delegate_macro_internal::uber_dangerous_transmute_this_is_unsound(
                    (self.handler)(
                        $crate::delegate::delegate_macro_internal::PhantomData,
                        $($crate::delegate::delegate_macro_internal::uber_dangerous_transmute_this_is_unsound($para_name),)*
                    )
                )
            }
        }

        impl<
            Func: 'static +
                $($(for<$($fn_lt),*>)?)?
                    Fn($($para),*) $(-> $ret)?
            $(, $($generic),*)?
        > $crate::delegate::delegate_macro_internal::From<Func> for $name $(<$($generic),*>)?
        $(where
            $($where_token)*
        )? {
            #[cfg_attr(debug_assertions, track_caller)]
            fn from(handler: Func) -> Self {
                Self::new(handler)
            }
        }

        impl<$($($generic,)*)? Marker, Handler: ?Sized> $crate::delegate::delegate_macro_internal::fmt::Debug for $name<$($($generic,)*)? Marker, Handler>
        $(where
            $($where_token)*
        )? {
            fn fmt(&self, fmt: &mut $crate::delegate::delegate_macro_internal::fmt::Formatter) -> $crate::delegate::delegate_macro_internal::fmt::Result {
                fmt.write_str("delegate::")?;
                fmt.write_str($crate::delegate::delegate_macro_internal::stringify!($name))?;
                fmt.write_str("(")?;
                $(
                    fmt.write_str($crate::delegate::delegate_macro_internal::stringify!($para))?;
                )*
                fmt.write_str(")")?;

                #[cfg(debug_assertions)]
                {
                    fmt.write_str(" @ ")?;
                    fmt.write_str(self.defined.file())?;
                    fmt.write_str(":")?;
                    $crate::delegate::delegate_macro_internal::fmt::Debug::fmt(&self.defined.line(), fmt)?;
                    fmt.write_str(":")?;
                    $crate::delegate::delegate_macro_internal::fmt::Debug::fmt(&self.defined.column(), fmt)?;
                }

                Ok(())
            }
        }

        impl<$($($generic,)*)? Marker, Handler: ?Sized> $crate::delegate::delegate_macro_internal::Clone for $name<$($($generic,)*)? Marker, Handler>
        $(where
            $($where_token)*
        )? {
            fn clone(&self) -> Self {
                Self {
                    _ty: (
                        $crate::delegate::delegate_macro_internal::PhantomData::<fn() -> Marker>,
                        $($($crate::delegate::delegate_macro_internal::PhantomData::<fn() -> $generic>,)*)?
                    ),
                    #[cfg(debug_assertions)]
                    defined: self.defined,
                    handler: $crate::delegate::delegate_macro_internal::Clone::clone(&self.handler),
                }
            }
        }

        impl<$($($generic,)*)? Marker, Handler: ?Sized> $crate::delegate::delegate_macro_internal::Delegate for $name<$($($generic,)*)? Marker, Handler>
        $(where
            $($where_token)*
        )?
        {
        }

        $crate::delegate::delegate! {
            @__internal_forward_derives

            $(#[$attr_meta])*
            $vis fn $name
                $(
                    <$($generic,)*>
                    $(<$($fn_lt,)*>)?
                )?
                ($($para_name: $para,)*) $(-> $ret)?
            $(as deriving $deriving $({ $($deriving_args)* })? )*
            $(where $($where_token)*)?
        }
    };

    // === Helpers === //
    (
        @__internal_forward_derives

        $(#[$attr_meta:meta])*
        $vis:vis fn $name:ident
            $(
                <$($generic:ident),* $(,)?>
                $(<$($fn_lt:lifetime),* $(,)?>)?
            )?
            ($($para_name:ident: $para:ty),* $(,)?) $(-> $ret:ty)?
        as deriving $first_deriving:path $({ $($first_deriving_args:tt)* })?
        $(as deriving $next_deriving:path $({ $($next_deriving_args:tt)* })? )*
        $(where $($where_token:tt)*)?
    ) => {
        $first_deriving! {
            args { $($($first_deriving_args)*)? }

            $(#[$attr_meta])*
            $vis fn $name
                $(
                    <$($generic,)*>
                    $(<$($fn_lt,)*>)?
                )?
                ($($para_name: $para,)*) $(-> $ret)?
            $(where $($where_token)*)?
        }

        $crate::delegate::delegate! {
            @__internal_forward_derives

            $(#[$attr_meta])*
            $vis fn $name
                $(
                    <$($generic,)*>
                    $(<$($fn_lt,)*>)?
                )?
                ($($para_name: $para,)*) $(-> $ret)?
            $(as deriving $next_deriving $({ $($next_deriving_args)* })?)*
            $(where $($where_token)*)?
        }
    };
    (
        @__internal_forward_derives

        $(#[$attr_meta:meta])*
        $vis:vis fn $name:ident
            $(
                <$($generic:ident),* $(,)?>
                $(<$($fn_lt:lifetime),* $(,)?>)?
            )?
            ($($para_name:ident: $para:ty),* $(,)?) $(-> $ret:ty)?
        $(where $($where_token:tt)*)?
    ) => { /* base case */};

    (@__internal_or_unit $ty:ty) => { $ty };
    (@__internal_or_unit) => { () };
}

pub use delegate;
