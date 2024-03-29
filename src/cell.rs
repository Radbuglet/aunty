use std::{
    cell::Cell,
    fmt,
    marker::PhantomData,
    mem::{forget, ManuallyDrop},
    ops::{Deref, DerefMut},
};

use crate::obj::Obj;

// === AuToken-Compatible RefCells === //

// TODO

// === update_cell === //

// CellOpener
pub trait CellOpener<T> {
    fn take(target: &Cell<T>) -> T;

    fn release(value: T, target: &Cell<T>);
}

#[derive(Debug, Copy, Clone, Default)]
pub struct NaturalCellOpener;

impl<T: CellOpenable> CellOpener<T> for NaturalCellOpener {
    fn take(target: &Cell<T>) -> T {
        T::take(target)
    }

    fn release(value: T, target: &Cell<T>) {
        T::release(value, target)
    }
}

#[derive(Debug, Copy, Clone, Default)]
pub struct DefaultCellOpener;

impl<T: Default> CellOpener<T> for DefaultCellOpener {
    fn take(target: &Cell<T>) -> T {
        target.take()
    }

    fn release(value: T, target: &Cell<T>) {
        target.set(value);
    }
}

#[derive(Debug, Copy, Clone, Default)]
pub struct CopyCellOpener;

impl<T: Copy> CellOpener<T> for CopyCellOpener {
    fn take(target: &Cell<T>) -> T {
        target.get()
    }

    fn release(value: T, target: &Cell<T>) {
        target.set(value);
    }
}

// CellOpenable
pub trait CellOpenable: Sized {
    fn take(target: &Cell<Self>) -> Self;

    fn release(self, target: &Cell<Self>) {
        target.set(self);
    }
}

impl<T> CellOpenable for Vec<T> {
    fn take(target: &Cell<Self>) -> Self {
        target.take()
    }

    fn release(self, target: &Cell<Self>) {
        forget(target.replace(self));
    }
}

impl<T> CellOpenable for Option<T> {
    fn take(target: &Cell<Self>) -> Self {
        target.take()
    }

    fn release(self, target: &Cell<Self>) {
        forget(target.replace(self));
    }
}

impl<T> CellOpenable for Obj<T> {
    fn take(target: &Cell<Self>) -> Self {
        target.take()
    }

    fn release(self, target: &Cell<Self>) {
        forget(target.replace(self));
    }
}

// Methods
pub struct CellOpenGuard<'a, T, S: CellOpener<T> = NaturalCellOpener> {
    _opener: PhantomData<fn() -> S>,
    cell: &'a Cell<T>,
    taken: ManuallyDrop<T>,
}

impl<T, S: CellOpener<T>> Deref for CellOpenGuard<'_, T, S> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        &self.taken
    }
}

impl<T, S: CellOpener<T>> DerefMut for CellOpenGuard<'_, T, S> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.taken
    }
}

impl<T, S: CellOpener<T>> Drop for CellOpenGuard<'_, T, S> {
    fn drop(&mut self) {
        S::release(unsafe { ManuallyDrop::take(&mut self.taken) }, self.cell);
    }
}

pub fn open_cell<T, S>(cell: &Cell<T>) -> CellOpenGuard<'_, T, S>
where
    S: CellOpener<T>,
{
    let taken = ManuallyDrop::new(S::take(cell));

    CellOpenGuard {
        _opener: PhantomData,
        cell,
        taken,
    }
}

// === CloneCell === //

#[repr(transparent)]
pub struct CloneCell<T, S = NaturalCellOpener> {
    _opener: PhantomData<fn() -> S>,
    cell: Cell<T>,
}

impl<T, S> CloneCell<T, S> {
    pub const fn new(value: T) -> Self {
        Self {
            _opener: PhantomData,
            cell: Cell::new(value),
        }
    }

    pub fn from_cell(cell: &Cell<T>) -> &CloneCell<T, S> {
        unsafe { std::mem::transmute(cell) }
    }

    pub fn from_mut(v: &mut T) -> &CloneCell<T, S> {
        Self::from_cell(Cell::from_mut(v))
    }

    pub fn cell(&self) -> &Cell<T> {
        &self.cell
    }

    pub fn as_ptr(&self) -> *mut T {
        self.cell.as_ptr()
    }

    pub fn get_mut(&mut self) -> &mut T {
        self.cell.get_mut()
    }

    pub fn into_inner(self) -> T {
        self.cell.into_inner()
    }

    pub fn set(&self, value: T) {
        self.cell.set(value);
    }

    pub fn replace(&self, value: T) -> T {
        self.cell.replace(value)
    }

    pub fn swap<S2>(&self, other: &CloneCell<T, S2>) {
        self.cell.swap(&other.cell)
    }

    pub fn take(&self) -> T
    where
        T: Default,
    {
        self.cell.take()
    }
}

impl<T, S: CellOpener<T>> CloneCell<T, S> {
    pub fn open(&self) -> CellOpenGuard<'_, T, S> {
        open_cell::<T, S>(&self.cell)
    }

    pub fn get(&self) -> T
    where
        T: Clone,
    {
        self.open().clone()
    }
}

impl<T: fmt::Debug + Clone, S: CellOpener<T>> fmt::Debug for CloneCell<T, S> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_tuple("CloneCell").field(&self.get()).finish()
    }
}

impl<T: Clone, S: CellOpener<T>> Clone for CloneCell<T, S> {
    fn clone(&self) -> Self {
        Self::new(self.get())
    }
}

impl<T: Default, S> Default for CloneCell<T, S> {
    fn default() -> Self {
        Self::new(T::default())
    }
}
