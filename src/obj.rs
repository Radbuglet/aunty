use std::{
    cell::{BorrowError, BorrowMutError, Ref, RefCell, RefMut},
    error::Error,
    fmt,
    mem::ManuallyDrop,
    ops::{Deref, DerefMut},
    ptr::NonNull,
    rc::{Rc, Weak},
};

use autoken::{
    ImmutableBorrow, MutableBorrow, Nothing, PotentialImmutableBorrow, PotentialMutableBorrow,
};

use crate::util::{coerce_weak, DebugUsingDisplay, FmtNoCycle, TransRc};

// === StrongObj === //

#[repr(transparent)]
pub struct StrongObj<T: ?Sized> {
    value: ManuallyDrop<TransRc<RefCell<T>>>,
}

impl<T: ?Sized + fmt::Debug> fmt::Debug for StrongObj<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self.value.get().try_borrow() {
            Ok(value) => f
                .debug_tuple("StrongObj")
                .field(&FmtNoCycle::<T>(&*value))
                .finish(),
            Err(err) => f
                .debug_tuple("StrongObj")
                .field(&DebugUsingDisplay(&err))
                .finish(),
        }
    }
}

impl<T: Default> Default for StrongObj<T> {
    fn default() -> Self {
        Self::new(T::default())
    }
}

impl<T> From<T> for StrongObj<T> {
    fn from(value: T) -> Self {
        Self::new(value)
    }
}

impl<T: ?Sized> Clone for StrongObj<T> {
    fn clone(&self) -> Self {
        Self {
            value: self.value.clone(),
        }
    }
}

impl<T: ?Sized> Eq for StrongObj<T> {}

impl<T: ?Sized> PartialEq for StrongObj<T> {
    fn eq(&self, other: &Self) -> bool {
        Weak::ptr_eq(self.value.as_weak(), other.value.as_weak())
    }
}

impl<T> StrongObj<T> {
    pub fn new(value: T) -> Self {
        Self {
            value: ManuallyDrop::new(Rc::new(RefCell::new(value)).into()),
        }
    }

    pub fn new_cyclic(f: impl FnOnce(&Obj<T>) -> T) -> Self {
        Self {
            value: ManuallyDrop::new(
                Rc::new_cyclic(|weak| {
                    RefCell::new(f(unsafe {
                        // Safety: `Obj<T>` is repr(transparent) w.r.t `Weak<T>`
                        std::mem::transmute(weak)
                    }))
                })
                .into(),
            ),
        }
    }
}

impl<T: ?Sized> StrongObj<T> {
    pub fn downgrade(&self) -> Obj<T> {
        Obj {
            value: self.value.as_weak().clone(),
        }
    }

    pub fn get(&self) -> ObjRef<T> {
        unsafe {
            // Safety: we can't drop the cell until it is mutably borrowable
            ObjRef::new_inner(ImmutableBorrow::new(), self.value.get().borrow())
        }
    }

    pub fn get_mut(&self) -> ObjMut<T> {
        unsafe {
            // Safety: we can't drop the cell until it is mutably borrowable
            ObjMut::new_inner(MutableBorrow::new(), self.value.get().borrow_mut())
        }
    }

    pub fn get_on_loan<'l>(&self, loaner: &'l ImmutableBorrow<T>) -> ObjRef<T, Nothing<'l>> {
        unsafe {
            // Safety: we can't drop the cell until it is mutably borrowable
            ObjRef::new_inner(loaner.loan(), self.value.get().borrow())
        }
    }

    pub fn get_mut_on_loan<'l>(&self, loaner: &'l mut MutableBorrow<T>) -> ObjMut<T, Nothing<'l>> {
        unsafe {
            // Safety: we can't drop the cell until it is mutably borrowable
            ObjMut::new_inner(loaner.loan(), self.value.get().borrow_mut())
        }
    }

    pub fn try_get<'l>(
        &self,
        loaner: &'l PotentialImmutableBorrow<T>,
    ) -> Result<ObjRef<T, Nothing<'l>>, BorrowError> {
        unsafe {
            // Safety: we can't drop the cell until it is mutably borrowable
            self.value
                .get()
                .try_borrow()
                .map(|guard| ObjRef::new_inner(loaner.loan(), guard))
        }
    }

    pub fn try_get_mut<'l>(
        &self,
        loaner: &'l mut PotentialMutableBorrow<T>,
    ) -> Result<ObjMut<T, Nothing<'l>>, BorrowMutError> {
        unsafe {
            // Safety: we can't drop the cell until it is mutably borrowable
            self.value
                .get()
                .try_borrow_mut()
                .map(|guard| ObjMut::new_inner(loaner.loan(), guard))
        }
    }

    pub fn coerce<V: ?Sized>(mut self, f: impl FnOnce(&RefCell<T>) -> &RefCell<V>) -> StrongObj<V> {
        let rc = unsafe {
            let taken = ManuallyDrop::take(&mut self.value);
            std::mem::forget(self);
            taken
        };

        StrongObj {
            value: ManuallyDrop::new(rc.coerce(f)),
        }
    }

    pub fn as_obj(&self) -> &Obj<T> {
        unsafe {
            // Safety: `Obj<T>` is repr(transparent) w.r.t `Weak<T>` and `Weak<T>` is always `Sized`.
            std::mem::transmute(self.value.as_weak())
        }
    }
}

impl<T: ?Sized> Drop for StrongObj<T> {
    fn drop(&mut self) {
        // Ensure that we're not about to drop an actively-borrowed value.
        if self.value.as_weak().strong_count() == 1 {
            if let Err(err) = self.value.get().try_borrow_mut() {
                panic!("attempted to drop StrongObj while in use: {err:?}");
            }
        }

        // We black-box the destructor to avoid false-positives due to weird MIR flags.
        autoken::assume_black_box(|| unsafe { ManuallyDrop::drop(&mut self.value) });
    }
}

// === Obj === //

#[repr(transparent)]
pub struct Obj<T: ?Sized> {
    value: Weak<RefCell<T>>,
}

impl<T: ?Sized + fmt::Debug> fmt::Debug for Obj<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self.value.upgrade() {
            Some(value) => match value.try_borrow() {
                Ok(value) => f
                    .debug_tuple("Obj")
                    .field(&FmtNoCycle::<T>(&*value))
                    .finish(),
                Err(err) => f
                    .debug_tuple("Obj")
                    .field(&DebugUsingDisplay(&err))
                    .finish(),
            },
            None => f
                .debug_tuple("Obj")
                .field(&DebugUsingDisplay(&WeakBorrowError::Dead))
                .finish(),
        }
    }
}

impl<T: ?Sized> Clone for Obj<T> {
    fn clone(&self) -> Self {
        Self {
            value: self.value.clone(),
        }
    }
}

impl<T: ?Sized> Eq for Obj<T> {}

impl<T: ?Sized> PartialEq for Obj<T> {
    fn eq(&self, other: &Self) -> bool {
        Weak::ptr_eq(&self.value, &other.value)
    }
}

impl<T> Default for Obj<T> {
    fn default() -> Self {
        Self::new()
    }
}

impl<T: ?Sized> Obj<T> {
    pub fn new() -> Self
    where
        T: Sized,
    {
        Self { value: Weak::new() }
    }

    pub fn is_alive(&self) -> bool {
        self.value.strong_count() > 0
    }

    pub fn try_upgrade(&self) -> Option<StrongObj<T>> {
        self.value.upgrade().map(|obj| StrongObj {
            value: ManuallyDrop::new(obj.into()),
        })
    }

    pub fn upgrade(&self) -> StrongObj<T> {
        self.try_upgrade()
            .expect("failed to upgrade obj: obj is dead")
    }

    unsafe fn try_upgrade_unguarded(&self) -> Option<&RefCell<T>> {
        self.is_alive().then(|| unsafe { &*self.value.as_ptr() })
    }

    unsafe fn upgrade_unguarded(&self) -> &RefCell<T> {
        assert!(self.is_alive(), "failed to get value from obj: obj is dead");
        unsafe { &*self.value.as_ptr() }
    }

    pub fn get(&self) -> ObjRef<T> {
        unsafe {
            // Safety: we have at least one `StrongObj` keeping this object alive and it cannot be
            // dropped until this `CompRef` is dropped.
            ObjRef::new_inner(ImmutableBorrow::new(), self.upgrade_unguarded().borrow())
        }
    }

    pub fn get_mut(&self) -> ObjMut<T> {
        unsafe {
            // Safety: we have at least one `StrongObj` keeping this object alive and it cannot be
            // dropped until this `CompRef` is dropped.
            ObjMut::new_inner(MutableBorrow::new(), self.upgrade_unguarded().borrow_mut())
        }
    }

    pub fn get_on_loan<'l>(&self, loaner: &'l ImmutableBorrow<T>) -> ObjRef<T, Nothing<'l>> {
        unsafe {
            // Safety: we have at least one `StrongObj` keeping this object alive and it cannot be
            // dropped until this `CompRef` is dropped.
            ObjRef::new_inner(loaner.loan(), self.upgrade_unguarded().borrow())
        }
    }

    pub fn get_mut_on_loan<'l>(&self, loaner: &'l mut MutableBorrow<T>) -> ObjMut<T, Nothing<'l>> {
        unsafe {
            // Safety: we have at least one `StrongObj` keeping this object alive and it cannot be
            // dropped until this `CompRef` is dropped.
            ObjMut::new_inner(loaner.loan(), self.upgrade_unguarded().borrow_mut())
        }
    }

    pub fn try_get<'l>(
        &self,
        loaner: &'l PotentialImmutableBorrow<T>,
    ) -> Result<ObjRef<T, Nothing<'l>>, WeakBorrowError> {
        unsafe {
            // Safety: we have at least one `StrongObj` keeping this object alive and it cannot be
            // dropped until this `CompRef` is dropped.
            let Some(cell) = self.try_upgrade_unguarded() else {
                return Err(WeakBorrowError::Dead);
            };

            match cell.try_borrow() {
                Ok(guard) => Ok(ObjRef::new_inner(loaner.loan(), guard)),
                Err(err) => Err(WeakBorrowError::Borrow(err)),
            }
        }
    }

    pub fn try_get_mut<'l>(
        &self,
        loaner: &'l mut PotentialMutableBorrow<T>,
    ) -> Result<ObjMut<T, Nothing<'l>>, WeakBorrowMutError> {
        unsafe {
            // Safety: we have at least one `StrongObj` keeping this object alive and it cannot be
            // dropped until this `CompRef` is dropped.
            let Some(cell) = self.try_upgrade_unguarded() else {
                return Err(WeakBorrowMutError::Dead);
            };

            match cell.try_borrow_mut() {
                Ok(guard) => Ok(ObjMut::new_inner(loaner.loan(), guard)),
                Err(err) => Err(WeakBorrowMutError::Borrow(err)),
            }
        }
    }

    pub fn coerce<V: ?Sized>(self, f: impl FnOnce(&RefCell<T>) -> &RefCell<V>) -> Obj<V> {
        Obj {
            value: coerce_weak(self.value, f),
        }
    }
}

// === AnyObj === //

// ExtendsObj Trait
pub trait ExtendsObj {
    type Wrapper: ObjWrapper<Wrapped = Self>;
}

pub unsafe trait ObjWrapper {
    type Wrapped: ?Sized;
}

#[doc(hidden)]
pub mod make_extensible_macro_internals {
    pub use super::{ExtendsObj, Obj, ObjWrapper};
}

#[macro_export]
macro_rules! make_extensible {
    (
        $vis:vis $extender:ident $(<$($lt:lifetime),* $(,)? $($para:ident)*>)? for $target:path
        $(where $($clauses:tt)*)?
    ) => {
        #[repr(transparent)]
        $vis struct $extender $(<$($lt,)* $($para)*>)?
		$(where $($clauses)*)?
		{
			$vis obj: $crate::obj::make_extensible_macro_internals::Obj<$target>,
		}

		impl $(< $($lt,)* $($para,)* >)? $crate::obj::make_extensible_macro_internals::ExtendsObj for $target $(< $($lt,)* $($para,)* >)?
		$(where $($clauses)*)?
		{
			type Wrapper = $extender $(< $($lt,)* $($para,)* >)?;
		}

		unsafe impl $(< $($lt,)* $($para,)* >)? $crate::obj::make_extensible_macro_internals::ObjWrapper for $extender $(< $($lt,)* $($para,)* >)?
		$(where $($clauses)*)?
		{
			type Wrapped = $target $(< $($lt,)* $($para,)* >)?;
		}
    };
}

pub use make_extensible;

// Deref Impls
impl<T: ?Sized + ExtendsObj> Deref for Obj<T> {
    type Target = T::Wrapper;

    fn deref(&self) -> &Self::Target {
        unsafe { std::mem::transmute(&self.value) }
    }
}

impl<T: ?Sized + ExtendsObj> Deref for StrongObj<T> {
    type Target = T::Wrapper;

    fn deref(&self) -> &Self::Target {
        self.as_obj()
    }
}

// === CompRef === //

pub struct ObjRef<T: ?Sized, B: ?Sized = T> {
    autoken: ImmutableBorrow<B>,
    guard: Ref<'static, ()>,
    value: NonNull<T>,
}

impl<T: ?Sized, B: ?Sized> ObjRef<T, B> {
    unsafe fn new_inner(autoken: ImmutableBorrow<B>, guard: Ref<'_, T>) -> Self {
        let value = NonNull::from(&*guard);
        let guard = Ref::map(guard, |_| &());
        let guard = std::mem::transmute(guard); // Erase lifetime

        Self {
            autoken,
            guard,
            value,
        }
    }

    #[allow(clippy::should_implement_trait)]
    pub fn clone(orig: &ObjRef<T, B>) -> ObjRef<T, B> {
        Self {
            autoken: orig.autoken.clone(),
            guard: Ref::clone(&orig.guard),
            value: orig.value,
        }
    }

    pub fn map<U, F>(orig: ObjRef<T, B>, f: F) -> ObjRef<U, B>
    where
        F: FnOnce(&T) -> &U,
        U: ?Sized,
    {
        let value = NonNull::from(f(&*orig));

        ObjRef {
            autoken: orig.autoken,
            guard: orig.guard,
            value,
        }
    }

    pub fn filter_map<U, F>(orig: ObjRef<T, B>, f: F) -> Result<ObjRef<U, B>, ObjRef<T, B>>
    where
        F: FnOnce(&T) -> Option<&U>,
        U: ?Sized,
    {
        if let Some(value) = f(&*orig) {
            let value = NonNull::from(value);

            Ok(ObjRef {
                autoken: orig.autoken,
                guard: orig.guard,
                value,
            })
        } else {
            Err(orig)
        }
    }

    pub fn map_split<U, V, F>(orig: ObjRef<T, B>, f: F) -> (ObjRef<U, B>, ObjRef<V, B>)
    where
        F: FnOnce(&T) -> (&U, &V),
        U: ?Sized,
        V: ?Sized,
    {
        let (left, right) = f(&*orig);
        let left = NonNull::from(left);
        let right = NonNull::from(right);

        (
            ObjRef {
                autoken: orig.autoken.clone(),
                guard: Ref::clone(&orig.guard),
                value: left,
            },
            ObjRef {
                autoken: orig.autoken,
                guard: orig.guard,
                value: right,
            },
        )
    }
}

impl<T: ?Sized, B: ?Sized> Deref for ObjRef<T, B> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        unsafe { self.value.as_ref() }
    }
}

impl<T: ?Sized + fmt::Debug, B: ?Sized> fmt::Debug for ObjRef<T, B> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        (**self).fmt(f)
    }
}

impl<T: ?Sized + fmt::Display, B: ?Sized> fmt::Display for ObjRef<T, B> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        (**self).fmt(f)
    }
}

// === CompMut === //

pub struct ObjMut<T: ?Sized, B: ?Sized = T> {
    autoken: MutableBorrow<B>,
    guard: RefMut<'static, ()>,
    value: NonNull<T>,
}

impl<T: ?Sized, B: ?Sized> ObjMut<T, B> {
    unsafe fn new_inner(autoken: MutableBorrow<B>, guard: RefMut<'_, T>) -> Self {
        let value = NonNull::from(&*guard);
        let guard = RefMut::map(guard, |_| Box::leak(Box::new(())));
        let guard = std::mem::transmute(guard); // Erase lifetime

        Self {
            autoken,
            guard,
            value,
        }
    }

    pub fn map<U, F>(mut orig: ObjMut<T, B>, f: F) -> ObjMut<U, B>
    where
        F: FnOnce(&mut T) -> &mut U,
        U: ?Sized,
    {
        let value = NonNull::from(f(&mut *orig));

        ObjMut {
            autoken: orig.autoken,
            guard: orig.guard,
            value,
        }
    }

    pub fn filter_map<U, F>(mut orig: ObjMut<T, B>, f: F) -> Result<ObjMut<U, B>, ObjMut<T, B>>
    where
        F: FnOnce(&mut T) -> Option<&U>,
        U: ?Sized,
    {
        if let Some(value) = f(&mut *orig) {
            let value = NonNull::from(value);

            Ok(ObjMut {
                autoken: orig.autoken,
                guard: orig.guard,
                value,
            })
        } else {
            Err(orig)
        }
    }

    pub fn map_split<U, V, F>(mut orig: ObjMut<T, B>, f: F) -> (ObjMut<U, B>, ObjMut<V, B>)
    where
        F: FnOnce(&mut T) -> (&mut U, &mut V),
        U: ?Sized,
        V: ?Sized,
    {
        let (left, right) = f(&mut *orig);
        let left = NonNull::from(left);
        let right = NonNull::from(right);

        let (left_guard, right_guard) = RefMut::map_split(orig.guard, |()| {
            (Box::leak(Box::new(())), Box::leak(Box::new(())))
        });

        (
            ObjMut {
                autoken: orig.autoken.assume_no_alias_clone(),
                guard: left_guard,
                value: left,
            },
            ObjMut {
                autoken: orig.autoken,
                guard: right_guard,
                value: right,
            },
        )
    }
}

impl<T: ?Sized, B: ?Sized> Deref for ObjMut<T, B> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        unsafe { self.value.as_ref() }
    }
}

impl<T: ?Sized, B: ?Sized> DerefMut for ObjMut<T, B> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        unsafe { self.value.as_mut() }
    }
}

impl<T: ?Sized + fmt::Debug, B: ?Sized> fmt::Debug for ObjMut<T, B> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        (**self).fmt(f)
    }
}

impl<T: ?Sized + fmt::Display, B: ?Sized> fmt::Display for ObjMut<T, B> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        (**self).fmt(f)
    }
}

// === WeakBorrowError === //

#[derive(Debug)]
pub enum WeakBorrowError {
    Dead,
    Borrow(BorrowError),
}

impl Error for WeakBorrowError {}

impl fmt::Display for WeakBorrowError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str("failed to borrow obj: ")?;

        match self {
            WeakBorrowError::Dead => f.write_str("obj is dead"),
            WeakBorrowError::Borrow(err) => fmt::Display::fmt(err, f),
        }
    }
}

#[derive(Debug)]
pub enum WeakBorrowMutError {
    Dead,
    Borrow(BorrowMutError),
}

impl Error for WeakBorrowMutError {}

impl fmt::Display for WeakBorrowMutError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str("failed to borrow obj: ")?;

        match self {
            WeakBorrowMutError::Dead => f.write_str("obj is dead"),
            WeakBorrowMutError::Borrow(err) => fmt::Display::fmt(err, f),
        }
    }
}
