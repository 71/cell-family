//! An [iterator](Iter) over [`Cell`] values.
use core::{iter::FusedIterator, marker::PhantomData};

use crate::{Cell, CellOwner, Family};

/// An iterator over the values in a stream of [`Cell`s](Cell).
pub struct Iter<'a, T: 'a, F: Family, I: Iterator<Item = &'a Cell<F, T>> + 'a> {
    owner: &'a CellOwner<F>,
    iter: I,
    _t: PhantomData<T>,
}

impl<'a, T: 'a, F: Family, I: Iterator<Item = &'a Cell<F, T>> + 'a> Iter<'a, T, F, I> {
    /// Creates a new [`Iter`] over the values of the given iterator.
    pub const fn new(owner: &'a CellOwner<F>, iter: I) -> Self {
        Self {
            owner,
            iter,
            _t: PhantomData,
        }
    }

    /// Returns a reference to the underlying [`CellOwner`] which proves that
    /// the yielded cells can be borrowed.
    pub const fn owner(&self) -> &'a CellOwner<F> {
        self.owner
    }

    /// Returns a reference to the underlying [`Iterator`] of cells.
    pub const fn inner(&self) -> &I {
        &self.iter
    }

    /// Returns the underlying iterator.
    pub fn into_inner(self) -> I {
        self.iter
    }
}

impl<'a, T: 'a, F: Family, I: Iterator<Item = &'a Cell<F, T>> + 'a> Iterator for Iter<'a, T, F, I> {
    type Item = &'a T;

    fn next(&mut self) -> Option<Self::Item> {
        Some(self.iter.next()?.get(self.owner))
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        self.iter.size_hint()
    }
}

impl<'a, T: 'a, F: Family, I: Iterator<Item = &'a Cell<F, T>> + 'a> DoubleEndedIterator
    for Iter<'a, T, F, I>
where
    I: DoubleEndedIterator,
{
    fn next_back(&mut self) -> Option<Self::Item> {
        Some(self.iter.next_back()?.get(self.owner))
    }
}

impl<'a, T: 'a, F: Family, I: Iterator<Item = &'a Cell<F, T>> + 'a> ExactSizeIterator
    for Iter<'a, T, F, I>
where
    I: ExactSizeIterator,
{
}

impl<'a, T: 'a, F: Family, I: Iterator<Item = &'a Cell<F, T>> + 'a> FusedIterator
    for Iter<'a, T, F, I>
where
    I: FusedIterator,
{
}

/// An iterator over the mutable references to values in a stream of
/// [`Cell`s](Cell).
///
/// Note that this iterator does _not_ actually implement [`Iterator`] since
/// returned values borrow the inner [`CellOwner`] mutably (i.e. exclusively).
/// Instead, [`next`](IterMut::next) must be called manually, as in the example
/// given in the documentation of [`CellOwner::iter_mut`].
pub struct IterMut<'a, T: 'a, F: Family, I: Iterator<Item = &'a Cell<F, T>> + 'a> {
    owner: &'a mut CellOwner<F>,
    iter: I,
    _t: PhantomData<T>,
}

impl<'a, T: 'a, F: Family, I: Iterator<Item = &'a Cell<F, T>> + 'a> IterMut<'a, T, F, I> {
    /// Creates a new [`Iter`] over the values of the given iterator.
    pub fn new(owner: &'a mut CellOwner<F>, iter: I) -> Self {
        Self {
            owner,
            iter,
            _t: PhantomData,
        }
    }

    /// Returns a reference to the underlying [`CellOwner`] which proves that
    /// the yielded cells can be borrowed.
    pub const fn owner(&self) -> &CellOwner<F> {
        &*self.owner
    }

    /// Returns a reference to the underlying [`CellOwner`] which proves that
    /// the yielded cells can be borrowed.
    pub fn owner_mut(&mut self) -> &mut CellOwner<F> {
        self.owner
    }

    /// Returns a reference to the underlying [`Iterator`] of cells.
    pub const fn inner(&self) -> &I {
        &self.iter
    }

    /// Returns the underlying iterator.
    pub fn into_inner(self) -> I {
        self.iter
    }
}

impl<'a, T: 'a, F: Family, I: Iterator<Item = &'a Cell<F, T>> + 'a> IterMut<'a, T, F, I> {
    /// Returns a mutable reference to the value inside the next cell yielded by
    /// the inner iterator.
    #[allow(clippy::should_implement_trait)]
    pub fn next(&mut self) -> Option<&mut T> {
        Some(self.iter.next()?.get_mut(self.owner))
    }
}

impl<'a, T: 'a, F: Family, I: Iterator<Item = &'a Cell<F, T>> + 'a> IterMut<'a, T, F, I>
where
    I: DoubleEndedIterator,
{
    /// Same as [`next`](IterMut::next), but starting at the end of the inner
    /// iterator.
    pub fn next_back(&mut self) -> Option<&mut T> {
        Some(self.iter.next_back()?.get_mut(self.owner))
    }
}
