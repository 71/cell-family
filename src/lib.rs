//! Cells inspired by [`qcell::TCell`][tcell] / [`qcell::TLCell`][tlcell], with
//! additional features.
//!
//! # Overview
//!
//! `cell-family` provides the [`define!`](crate::define) macro, which defines a
//! new [`Family`]. For each family, a corresponding [`Cell`] and [`CellOwner`]
//! can be created. Only a single [`CellOwner`] per family can exist at once,
//! but multiple cells can exist at the same time.
//!
//! For instance, you may define a family `FooFamily` as below:
//!
//! ```
//! # #![cfg_attr(feature = "nightly", feature(thread_local))]
//! cell_family::define!(type FooFamily: FooCellOwner for FooCell<T>);
//! ```
//!
//! This defines `FooFamily` (which implements [`Family`]) as well as
//! `FooCellOwner` and `FooCell<T>`, aliases for [`CellOwner<FooFamily>`] and
//! [`Cell<FooFamily, T>`] respectively.
//!
//! One `FooCellOwner` can exist per thread, and thus `FooCellOwner` is **not**
//! `Send`, since sending a `FooCellOwner` to another thread may allow two
//! `FooCellOwner`s to co-exist in a single thread. To allow a single
//! `FooCellOwner` per program (and thus make `FooCellOwner` `Send`),
//! prefix `define!` with `static`:
//!
//! ```
//! cell_family::define!(static type FooFamily: FooCellOwner for FooCell<T>);
//! ```
//!
//! For both thread-local and thread-safe families, the API is the same:
//!
//! ```
//! # #![cfg_attr(feature = "nightly", feature(thread_local))]
//! # cell_family::define!(type FooFamily: FooCellOwner for FooCell<T>);
//! let mut owner = FooCellOwner::new();
//! let a = FooCell::new(1);
//! let b = FooCell::new("bar");
//!
//! assert_eq!(*a.get(&owner), 1);
//! assert_eq!(*b.get(&owner), "bar");
//!
//! *a.get_mut(&mut owner) += 1;
//! *b.get_mut(&mut owner) = "baz";
//!
//! assert_eq!(*a.get(&owner), 2);
//! assert_eq!(*b.get(&owner), "baz");
//! ```
//!
//! - [`FooCell::new(T)`](Cell::new) simple wraps `T` in a
//!   `#[repr(transparent)]` [`FooCell`](Cell) without performing any checks.
//! - [`FooCell::get(&FooCellOwner)`](Cell::get) and
//!   [`FooCell::get_mut(&mut FooCellOwner)`](Cell::get_mut) are constant-time
//!   operations that return `&T` and `&mut T` respectively without performing
//!   any runtime checks. Since a single [`FooCellOwner`](CellOwner) exists per
//!   program (or thread), the aliasing rules of each cell is enforced by Rust
//!   through the `FooCellOwner`, which is borrowed as long as each `FooCell` is
//!   borrowed.
//! - `FooFamily` ensures that a single `FooCellOwner` exists within a program;
//!   if another `FooCellOwner` exists, [`FooCellOwner::new()`](CellOwner::new)
//!   will panic. A [`try_new()`](CellOwner::try_new) counterpart exists to
//!   avoid crashing in such a case.
//!
//! # Benefits over [`qcell::TCell`][tcell] / [`qcell::TLCell`][tlcell]
//!
//! - Unlike [`qcell::TCell`][tcell] (respectively [`qcell::TLCell`][tlcell]),
//!   the `Family` `F` is in charge of ensuring that a single `CellOwner<F>`
//!   exists per program (respectively thread). By using macros to generate
//!   families, we only need a single
//!   [`AtomicBool`](std::sync::atomic::AtomicBool) (respectively
//!   [`Cell<bool>`](std::cell::Cell)) for each family, thus requiring no
//!   allocations.
//! - A few additional methods are provided; for instance,
//!   [`owner.get(c)`](CellOwner::get), [`owner.get_mut(c)`](CellOwner::get_mut)
//!   and [`owner.try_get_mut(c)`](CellOwner::try_get_mut) are provided, where
//!   `c` can be:
//!   * A tuple of [`Cell`]s.
//!   * An array of [`Cell`]s.
//!   * An array of references to [`Cell`]s.
//!   * A slice of [`Cell`]s.
//! - Thread-local and thread-safe [`Cell`]s (and [`CellOwner`]s) are backed by
//!   the same type; whether they are thread-local or thread-safe is determined
//!   by their [`Family`]: if it is thread-safe, it will also implement
//!   [`ThreadSafeFamily`]. This makes it easier to define generic functions
//!   over `Cell`s.
//! - `cell-family` fully supports `#[no_std]`, **except** for thread-local
//!   families in non-`nightly` builds (since thread-local variables cannot be
//!   defined in `#[no_std]` without `#[thread_local]`, which is not stable).
//! - `Cell` is [`Debug`](std::fmt::Debug), and will print a representation of
//!   its inner value if no `CellOwner` currently exists.
//!
//! [tcell]: https://docs.rs/qcell/0.5.2/qcell/struct.TCell.html
//! [tlcell]: https://docs.rs/qcell/0.5.2/qcell/struct.TLCell.html

#![cfg_attr(not(feature = "std"), no_std)]
#![cfg_attr(
    feature = "nightly",
    feature(maybe_uninit_array_assume_init, maybe_uninit_uninit_array, new_uninit)
)]

#[rustversion::nightly]
extern crate alloc;

use core::{cell::UnsafeCell, marker::PhantomData};

/// Re-export of `#[rustversion(...)]`, which is used in macro-generated code.
#[doc(hidden)]
pub use rustversion;

mod define;
mod impls;

pub mod iter;

/// A cell whose mutability is determined through its [`CellOwner`].
///
/// # Example
/// ```
/// # #![cfg_attr(feature = "nightly", feature(thread_local))]
/// # cell_family::define!(type Family: CellOwner for Cell<T>);
/// let cell = Cell::new(vec![1, 2, 3]);
/// let mut owner = CellOwner::new();
///
/// cell.get_mut(&mut owner).push(4);
///
/// assert_eq!(cell.get(&owner), &[1, 2, 3, 4]);
/// ```
#[repr(transparent)]
pub struct Cell<F: Family, T: ?Sized> {
    _family: PhantomData<Invariant<F>>,
    value: UnsafeCell<T>,
}

// SAFETY: see
// https://github.com/uazu/qcell/blob/0b69b7953d420612de58dd433e94681690bbd20c/src/tcell.rs#L252-L267.
unsafe impl<F: ThreadSafeFamily, T: Send + Sync + ?Sized> Sync for Cell<F, T> {}

impl<F: Family, T> Cell<F, T> {
    /// Creates a new cell which wraps the given value, and whose value can be
    /// borrowed by a [`CellOwner`].
    pub const fn new(value: T) -> Self {
        Self {
            value: UnsafeCell::new(value),
            _family: PhantomData,
        }
    }

    /// Returns the inner value, consuming the cell.
    pub fn into_inner(self) -> T {
        self.value.into_inner()
    }
}

impl<F: Family, T: ?Sized> Cell<F, T> {
    /// Returns a reference to the underlying value of the cell, using the
    /// reference to the given [`CellOwner`] as a proof that this cell is not
    /// currently mutably borrowed.
    pub fn get<'a>(&'a self, owner: &'a CellOwner<F>) -> &'a T {
        let _ = owner;

        // SAFETY: `self`'s borrow state is determined by `owner`'s.
        unsafe { self.get_unchecked() }
    }

    /// Returns a reference to the underlying value of the cell without any
    /// checks.
    ///
    /// # Safety
    /// When calling this method, you have to ensure that the current cell (and
    /// its underlying data) is not borrowed mutably for the entire lifetime of
    /// the returned reference.
    pub unsafe fn get_unchecked(&self) -> &T {
        &*self.value.get()
    }

    /// Returns a reference to the underlying value of the cell, using the
    /// reference to the given [`CellOwner`] as a proof that this cell is not
    /// currently mutably borrowed.
    pub fn get_mut<'a>(&'a self, owner: &'a mut CellOwner<F>) -> &'a mut T {
        let _ = owner;

        // SAFETY: `self`'s borrow state is determined by `owner`'s.
        unsafe { self.get_unchecked_mut() }
    }

    /// Returns a mutable reference to the underlying value of the cell without
    /// any checks.
    ///
    /// # Safety
    /// When calling this method, you have to ensure that the current cell (and
    /// its underlying data) is not borrowed anywhere else for the entire
    /// lifetime of the returned reference.
    #[allow(clippy::mut_from_ref)]
    pub unsafe fn get_unchecked_mut(&self) -> &mut T {
        &mut *self.value.get()
    }

    /// Clones the inner value and returns a [`Cell`] wrapping it.
    pub fn clone(&self, owner: &CellOwner<F>) -> Self
    where
        T: Clone,
    {
        Self::new(self.get(owner).clone())
    }

    /// Copies the inner value and returns a [`Cell`] wrapping it.
    pub fn copy(&self, owner: &CellOwner<F>) -> Self
    where
        T: Copy,
    {
        Self::new(*self.get(owner))
    }
}

impl<F: Family, T: Default> Default for Cell<F, T> {
    fn default() -> Self {
        Self::new(Default::default())
    }
}

impl<F: Family, T> From<T> for Cell<F, T> {
    fn from(value: T) -> Self {
        Self::new(value)
    }
}

/// Owner of zero or more [`Cell`s](Cell).
pub struct CellOwner<F: Family> {
    // See https://github.com/uazu/qcell/blob/0b69b7953d420612de58dd433e94681690bbd20c/src/tlcell.rs#L19.
    _marker: PhantomData<(NotSendOrSync, Invariant<F>)>,
}

// SAFETY: when `F` is `Family`, only one `CellOwner<F>` can be created per
// program, so it is then safe to send it (or a reference to it) to other
// threads.
unsafe impl<F: ThreadSafeFamily> Send for CellOwner<F> {}
unsafe impl<F: ThreadSafeFamily> Sync for CellOwner<F> {}

impl<F: Family> Drop for CellOwner<F> {
    fn drop(&mut self) {
        F::unborrow_owner();
    }
}

impl<F: Family> CellOwner<F> {
    /// Creates the unique [`CellOwner<F>`] in the current thread, panicking if
    /// another [`CellOwner<F>`] already exists in this thread.
    #[allow(clippy::new_without_default)]
    pub fn new() -> Self {
        if let Err(error) = F::borrow_owner() {
            panic!("{}", error);
        }

        CellOwner {
            _marker: PhantomData,
        }
    }

    /// Creates the unique [`CellOwner<F>`] in the current thread, returning
    /// [`None`] if another [`CellOwner<F>`] already exists in this thread.
    pub fn try_new() -> Option<Self> {
        F::borrow_owner().ok().map(|()| CellOwner {
            _marker: PhantomData,
        })
    }

    /// Creates a [`CellOwner<F>`] without checking whether another
    /// [`CellOwner<F>`] already exists in this thread.
    ///
    /// # Safety
    /// When calling this method, you have to ensure that no other [`CellOwner`]
    /// exists in this thread (for [thread-local families](PerThreadFamily)) or
    /// in the entire program (for [program families](Family)).
    pub unsafe fn new_unchecked() -> Self {
        debug_assert!(F::borrow_owner().is_ok());

        CellOwner {
            _marker: PhantomData,
        }
    }

    /// Equivalent to `value.get(&self)` for a [`Cell`],
    /// `(item.get(&self), ...)` for a tuple of [`Cell`s](Cell), or
    /// `[item.get(&self), ...)` for an array or slice of [`Cell`s](Cell).
    ///
    /// # Example
    /// ```
    /// # #![cfg_attr(feature = "nightly", feature(thread_local))]
    /// # cell_family::define!(type Family: CellOwner for Cell<T>);
    /// let a = Cell::new(vec![1, 2, 3]);
    /// let b = Cell::new(4);
    /// let owner = CellOwner::new();
    ///
    /// assert_eq!(owner.get((&a, &b)), (&vec![1, 2, 3], &4));
    /// ```
    pub fn get<'a, T: GetWithOwner<'a, F>>(&'a self, value: T) -> T::Get {
        value.get(self)
    }

    /// Equivalent to `value.get_mut(&mut self)` for a [`Cell`],
    /// `(item.get_mut(&mut self), ...)` for a tuple of [`Cell`s](Cell), or
    /// `[item.get_mut(&mut self), ...]` for an array or slice of
    /// [`Cell`s](Cell).
    ///
    /// Panics if multiple cells point to the same location.
    ///
    /// # Example
    /// ```
    /// # #![cfg_attr(feature = "nightly", feature(thread_local))]
    /// # cell_family::define!(type Family: CellOwner for Cell<T>);
    /// let a = Cell::new(vec![1, 2, 3]);
    /// let b = Cell::new(vec![4, 5, 6]);
    /// let mut owner = CellOwner::new();
    ///
    /// assert_eq!(owner.get_mut([&a, &b]), [&[1, 2, 3], &[4, 5, 6]]);
    /// ```
    ///
    /// # Example
    /// ```
    /// # #![cfg_attr(feature = "nightly", feature(thread_local))]
    /// # cell_family::define!(type Family: CellOwner for Cell<T>);
    /// let mut owner = CellOwner::new();
    /// let arr = [Cell::new(1), Cell::new(2)];
    ///
    /// for v in owner.get_mut(&arr) {
    ///     *v += 1;
    /// }
    ///
    /// assert_eq!(owner.get(&arr), [&2, &3]);
    /// ```
    ///
    /// # Example
    /// ```
    /// # #![cfg_attr(feature = "nightly", feature(thread_local))]
    /// # cell_family::define!(type Family: CellOwner for Cell<T>);
    /// let mut owner = CellOwner::new();
    /// let a = Cell::new(1);
    /// let b = Cell::new(2);
    ///
    /// for v in owner.get_mut([&a, &b]) {
    ///     *v += 1;
    /// }
    ///
    /// assert_eq!(*a.get(&owner), 2);
    /// assert_eq!(*b.get(&owner), 3);
    /// ```
    pub fn get_mut<'a, T: GetMutWithOwner<'a, F>>(&'a mut self, value: T) -> T::GetMut {
        value.get_mut(self)
    }

    /// Equivalent to `value.get_mut(&mut self)` for a [`Cell`],
    /// `(item.get_mut(&mut self), ...)` for a tuple of [`Cell`s](Cell), or
    /// `[item.get_mut(&mut self), ...]` for an array or slice of
    /// [`Cell`s](Cell).
    ///
    /// Returns [`None`] if multiple cells point to the same location.
    ///
    /// # Example
    /// ```
    /// # #![cfg_attr(feature = "nightly", feature(thread_local))]
    /// # cell_family::define!(type Family: CellOwner for Cell<T>);
    /// let a = Cell::new(1);
    /// let b = Cell::new("world");
    /// let mut owner = CellOwner::new();
    ///
    /// assert_eq!(owner.try_get_mut((&a, &b)), Some((&mut 1, &mut "world")));
    /// assert_eq!(owner.try_get_mut((&a, &a)), None);
    /// ```
    ///
    /// # Example
    /// ```
    /// # #![cfg_attr(feature = "nightly", feature(thread_local))]
    /// # cell_family::define!(type Family: CellOwner for Cell<T>);
    /// let mut owner = CellOwner::new();
    /// let arr = [Cell::new(()), Cell::new(())];
    ///
    /// assert_eq!(owner.try_get_mut(&arr), None);
    /// ```
    pub fn try_get_mut<'a, T: GetMutWithOwner<'a, F>>(&'a mut self, value: T) -> Option<T::GetMut> {
        value.try_get_mut(self)
    }

    /// Returns an [`Iterator`] over the inner references to the cells yielded
    /// by `iterator`.
    ///
    /// # Example
    /// ```
    /// # #![cfg_attr(feature = "nightly", feature(thread_local))]
    /// # cell_family::define!(type Family: CellOwner for Cell<T>);
    /// let cells = [Cell::new(1), Cell::new(2)];
    /// let owner = CellOwner::new();
    /// let mut iter = owner.iter(&cells);
    ///
    /// assert_eq!(iter.next(), Some(&1));
    /// assert_eq!(iter.next(), Some(&2));
    /// assert_eq!(iter.next(), None);
    /// ```
    pub fn iter<'a, T: 'a, I: Iterator<Item = &'a Cell<F, T>> + 'a>(
        &'a self,
        iterator: impl IntoIterator<IntoIter = I>,
    ) -> iter::Iter<'a, T, F, I> {
        iter::Iter::new(self, iterator.into_iter())
    }

    /// Returns an [iterator](iter::IterMut) over the inner mutable references
    /// yielded by `iterator`.
    ///
    /// # Example
    /// ```
    /// # #![cfg_attr(feature = "nightly", feature(thread_local))]
    /// # cell_family::define!(type Family: CellOwner for Cell<T>);
    /// let cells = [Cell::new(1), Cell::new(2)];
    /// let mut owner = CellOwner::new();
    /// let mut iter = owner.iter_mut(&cells);
    ///
    /// while let Some(v) = iter.next() {
    ///     *v += 1;
    /// }
    ///
    /// assert_eq!(owner.get(&cells), [&2, &3]);
    /// ```
    pub fn iter_mut<'a, T: 'a, I: Iterator<Item = &'a Cell<F, T>> + 'a>(
        &'a mut self,
        iterator: impl IntoIterator<IntoIter = I>,
    ) -> iter::IterMut<'a, T, F, I> {
        iter::IterMut::new(self, iterator.into_iter())
    }
}

/// `F` marker type in [`Cell`] and [`CellOwner`].
///
/// Note that this trait does not enable usage of [`CellOwner`s](CellOwner) from
/// multiple threads; [`ThreadSafeFamily`] (generated with the `static` keyword
/// in [`define!`]) is required to enable [`CellOwner`]`: Send + Sync`.
///
/// # Safety
/// This trait is automatically implemented by [`define!`] since it validates
/// invariants required for [`CellOwner`] and [`Cell`] to be safe to use. Do not
/// implement.
pub unsafe trait Family: 'static {
    /// The name of the family, used for debugging.
    const NAME: &'static str;

    /// Statically marks the thread-local or global [`CellOwner`] as borrowed,
    /// returning an error if it is already borrowed.
    fn borrow_owner() -> Result<(), &'static str>;

    /// Statically releases the borrow on the thread-local or global
    /// [`CellOwner`] following a successful call to
    /// [`borrow_owner()`](Family::borrow_owner).
    fn unborrow_owner();
}

/// An extension of [`Family`] to define [`CellOwner`s](CellOwner) that can be
/// shared between different threads, at the cost of slighly more expensive
/// costs to [create](CellOwner::new) or [drop](Drop) a [`CellOwner`].
///
/// # Safety
/// This trait is automatically implemented by [`define!`] since it validates
/// invariants required for [`CellOwner`] and [`Cell`] to be safe to use. Do not
/// implement.
pub unsafe trait ThreadSafeFamily: Family {}

pub(crate) mod private {
    /// Trait that cannot be implemented by external traits, required to
    /// implement [`super::GetWithOwner`] and [`super::GetMutWithOwner`].
    pub trait Sealed<F: super::Family> {}
}

/// Trait implemented by values that can be given to [`CellOwner::get()`].
pub trait GetWithOwner<'a, F: Family>: Sized + private::Sealed<F> {
    /// The result of [`Self::get()`].
    type Get;

    /// Returns a reference to the requested value(s).
    fn get(self, owner: &'a CellOwner<F>) -> Self::Get;
}

/// Trait implemented by values that can be given to [`CellOwner::get_mut()`].
pub trait GetMutWithOwner<'a, F: Family>: GetWithOwner<'a, F> {
    /// The result of [`Self::get_mut()`].
    type GetMut;

    /// Returns a mutable reference to the requested value(s), returning
    /// [`None`] instead if any of them is not unique (i.e. referenced by two
    /// different cells).
    fn try_get_mut(self, owner: &'a mut CellOwner<F>) -> Option<Self::GetMut>;

    /// Same as [`Self::try_get_mut()`], crashing on error.
    fn get_mut(self, owner: &'a mut CellOwner<F>) -> Self::GetMut {
        self.try_get_mut(owner)
            .expect("more than one cell point to the same location")
    }
}

/// See [`qcell::Invariant`](
/// https://github.com/uazu/qcell/blob/0b69b7953d420612de58dd433e94681690bbd20c/src/lib.rs#L387-L406).
struct Invariant<T>(fn(T) -> T);

/// A type that's neither [`Send`] nor [`Sync`], which will propagate to any
/// `struct` that contains it (including in a [`PhantomData`]).
struct NotSendOrSync(*const ());
