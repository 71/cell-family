use core::{fmt::Debug, mem::MaybeUninit};

use crate::{private, Cell, CellOwner, Family, GetMutWithOwner, GetWithOwner};

/// Simple [`Debug`] implementation for [`CellOwner`].
///
/// # Example
/// ```
/// # #![cfg_attr(feature = "nightly", feature(thread_local))]
/// # cell_family::define!(type Family: CellOwner);
/// let owner = CellOwner::new();
///
/// assert_eq!(format!("{owner:?}"), "CellOwner { family: \"Family\" }");
/// ```
impl<F: Family> Debug for CellOwner<F> {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        f.debug_struct("CellOwner")
            .field("family", &F::NAME)
            .finish()
    }
}

/// Simple [`Debug`] implementation for [`Cell`].
///
/// # Example
/// ```
/// # #![cfg_attr(feature = "nightly", feature(thread_local))]
/// # cell_family::define!(type Family: CellOwner for Cell<T>);
/// let cell = Cell::new(vec![1, 2, 3]);
/// let owner = CellOwner::new();
///
/// assert_eq!(format!("{cell:?}"), "Cell { family: \"Family\", .. }");
///
/// drop(owner);
///
/// assert_eq!(format!("{cell:?}"), "Cell { family: \"Family\", value: [1, 2, 3] }");
/// ```
impl<F: Family, T: Debug + ?Sized> Debug for Cell<F, T> {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        if let Some(owner) = CellOwner::try_new() {
            f.debug_struct("Cell")
                .field("family", &F::NAME)
                .field("value", &self.get(&owner))
                .finish()
        } else {
            f.debug_struct("Cell")
                .field("family", &F::NAME)
                .finish_non_exhaustive()
        }
    }
}

impl<F: Family, T: ?Sized> private::Sealed<F> for &Cell<F, T> {}

impl<'a, F: Family, T: ?Sized> GetWithOwner<'a, F> for &'a Cell<F, T> {
    type Get = &'a T;

    fn get(self, owner: &'a CellOwner<F>) -> Self::Get {
        self.get(owner)
    }
}

impl<'a, F: Family, T: ?Sized> GetMutWithOwner<'a, F> for &'a Cell<F, T> {
    type GetMut = &'a mut T;

    fn try_get_mut(self, owner: &'a mut CellOwner<F>) -> Option<Self::GetMut> {
        Some(self.get_mut(owner))
    }

    fn get_mut(self, owner: &'a mut CellOwner<F>) -> Self::GetMut {
        self.get_mut(owner)
    }
}

impl<F: Family, T, const N: usize> private::Sealed<F> for [&Cell<F, T>; N] {}

impl<'a, F: Family, T, const N: usize> GetWithOwner<'a, F> for [&'a Cell<F, T>; N] {
    type Get = [&'a T; N];

    fn get(self, owner: &'a CellOwner<F>) -> Self::Get {
        let mut result = uninit_array();

        for i in 0..self.len() {
            result[i].write(self[i].get(owner));
        }

        // SAFETY: all items were initialized in the loop above.
        unsafe { array_assume_init(result) }
    }
}

impl<'a, F: Family, T, const N: usize> GetMutWithOwner<'a, F> for [&'a Cell<F, T>; N] {
    type GetMut = [&'a mut T; N];

    fn try_get_mut(self, owner: &'a mut CellOwner<F>) -> Option<Self::GetMut> {
        let _ = owner;
        let mut result = uninit_array();

        for i in 0..self.len() {
            let item = self[i];

            for further_item in self.iter().copied().skip(i + 1) {
                if core::ptr::eq(item, further_item) {
                    return None;
                }
            }

            // SAFETY: we do have a mutably-borrowed `owner` here, and we made
            // sure with the inner loop that there is no other mutable borrow
            // of `item`.
            result[i].write(unsafe { item.get_unchecked_mut() });
        }

        // SAFETY: all items were initialized above.
        Some(unsafe { array_assume_init(result) })
    }
}

impl<F: Family, T, const N: usize> private::Sealed<F> for &[Cell<F, T>; N] {}

impl<'a, F: Family, T, const N: usize> GetWithOwner<'a, F> for &'a [Cell<F, T>; N] {
    type Get = [&'a T; N];

    fn get(self, owner: &'a CellOwner<F>) -> Self::Get {
        let _ = owner;
        let mut result = uninit_array();

        for i in 0..self.len() {
            result[i].write(self[i].get(owner));
        }

        // SAFETY: all items were initialized in the loop above.
        unsafe { array_assume_init(result) }
    }
}

impl<'a, F: Family, T, const N: usize> GetMutWithOwner<'a, F> for &'a [Cell<F, T>; N] {
    type GetMut = [&'a mut T; N];

    fn try_get_mut(self, owner: &'a mut CellOwner<F>) -> Option<Self::GetMut> {
        // Edge case: each item in a slice of ZSTs point to the same memory
        // address, and referencing this address more than once would lead to
        // aliasing.
        if core::mem::size_of::<T>() == 0 && self.len() > 1 {
            return None;
        }

        let _ = owner;
        let mut result = uninit_array();

        for i in 0..self.len() {
            // SAFETY: the mutable reference to `owner` proves that no cell
            // contents in this family are mutably borrowed. Since every cell in
            // the current array is guaranteed to have a different address, we
            // can borrow simultaneously from different cells.
            result[i].write(unsafe { self[i].get_unchecked_mut() });
        }

        // SAFETY: all items were initialized in the loop above.
        Some(unsafe { array_assume_init(result) })
    }
}

#[inline]
fn uninit_array<T, const N: usize>() -> [MaybeUninit<T>; N] {
    // We must wrap the body in a macro since putting `const { ... }` in a function is invalid in
    // older versions of Rust, even if this function is erased by `#[rustversion]`.
    #[rustversion::since(1.80)]
    macro_rules! body {
        () => {
            [const { MaybeUninit::uninit() }; N]
        };
    }
    #[rustversion::before(1.80)]
    macro_rules! body {
        () => {
            // SAFETY: an uninitialized `[MaybeUninit<T>; N]` is valid.
            unsafe { MaybeUninit::<[MaybeUninit<T>; N]>::uninit().assume_init() }
        };
    }

    body!()
}

#[inline]
unsafe fn array_assume_init<T, const N: usize>(array: [MaybeUninit<T>; N]) -> [T; N] {
    #[cfg(feature = "nightly")]
    {
        MaybeUninit::array_assume_init(array)
    }
    #[cfg(not(feature = "nightly"))]
    {
        // SAFETY: we own `array` (ensuring no aliasing), and we will drop it as an array of uninit
        // values, hence we will not touch its contents again.
        (&array as *const _ as *const [T; N]).read()
    }
}

#[rustversion::since(1.82)]
impl<F: Family, T> private::Sealed<F> for &[Cell<F, T>] {}

#[rustversion::since(1.82)]
impl<'a, F: Family, T> GetWithOwner<'a, F> for &'a [Cell<F, T>] {
    type Get = alloc::boxed::Box<[&'a T]>;

    #[rustversion::attr(since(1.81), expect(clippy::incompatible_msrv))]
    fn get(self, owner: &'a CellOwner<F>) -> Self::Get {
        let _ = owner;
        let mut result = alloc::boxed::Box::new_uninit_slice(self.len());

        for i in 0..self.len() {
            result[i].write(self[i].get(owner));
        }

        // SAFETY: all items were initialized in the loop above.
        unsafe { result.assume_init() }
    }
}

#[rustversion::since(1.82)]
impl<'a, F: Family, T> GetMutWithOwner<'a, F> for &'a [Cell<F, T>] {
    type GetMut = alloc::boxed::Box<[&'a mut T]>;

    #[rustversion::attr(since(1.81), expect(clippy::incompatible_msrv))]
    fn try_get_mut(self, owner: &'a mut CellOwner<F>) -> Option<Self::GetMut> {
        // Edge case: each item in a slice of ZSTs point to the same memory
        // address, and referencing this address more than once would lead to
        // aliasing.
        if core::mem::size_of::<T>() == 0 && self.len() > 1 {
            return None;
        }

        let _ = owner;
        let mut result = alloc::boxed::Box::new_uninit_slice(self.len());

        for i in 0..self.len() {
            // SAFETY: the mutable reference to `owner` proves that no cell
            // contents in this family are mutably borrowed. Since every cell in
            // the current array is guaranteed to have a different address, we
            // can borrow simultaneously from different cells.
            result[i].write(unsafe { self[i].get_unchecked_mut() });
        }

        // SAFETY: all items were initialized in the loop above.
        Some(unsafe { result.assume_init() })
    }
}

#[inline]
fn are_unique<const N: usize>(pointers: [*const u8; N]) -> bool {
    // O(n log n) check.
    for i in 0..pointers.len() {
        for j in i + 1..pointers.len() {
            if core::ptr::eq(pointers[i], pointers[j]) {
                return false;
            }
        }
    }

    true
}

macro_rules! is_unique {
    () => {
        // No value: no-op.
        true
    };

    ( $unique: ident, ) => {
        // Only one value: no-op.
        true
    };

    ( $a: ident, $b: ident, ) => {
        // Two values: direct comparison.
        !core::ptr::eq($a, $b)
    };

    ( $($values: ident),* ) => {
        // More values: generic check.
        are_unique([$($values as *const _ as *const u8),*])
    };
}

macro_rules! impl_get_for_tuple {
    (
        $($name: ident),*
    ) => {
        #[allow(unused_parens, clippy::unused_unit)]
        impl<'a, CF: Family, $($name: ?Sized),*> private::Sealed<CF> for ($(&'a Cell<CF, $name>,)*) {}

        #[allow(unused_parens, clippy::unused_unit)]
        impl<'a, CF: Family, $($name: ?Sized),*> GetWithOwner<'a, CF> for ($(&'a Cell<CF, $name>,)*) {
            type Get = ($(&'a $name,)*);

            #[allow(non_snake_case, unused_variables)]
            fn get(self, owner: &'a CellOwner<CF>) -> Self::Get {
                let ($($name,)*) = self;

                ($($name.get(owner),)*)
            }
        }

        #[allow(unused_parens, clippy::unused_unit)]
        impl<'a, CF: Family, $($name: ?Sized),*> GetMutWithOwner<'a, CF> for ($(&'a Cell<CF, $name>,)*) {
            type GetMut = ($(&'a mut $name,)*);

            #[allow(non_snake_case, unused_unsafe, clippy::multiple_unsafe_ops_per_block)]
            fn try_get_mut(self, _owner: &'a mut CellOwner<CF>) -> Option<Self::GetMut> {
                let ($($name,)*) = self;

                if !is_unique!($($name),*) {
                    return None;
                }

                // SAFETY: all references are different, so we're not
                // introducing any aliasing.
                Some(unsafe {
                    ($($name.get_unchecked_mut(),)*)
                })
            }
        }
    };
}

impl_get_for_tuple!();
impl_get_for_tuple!(A);
impl_get_for_tuple!(A, B);
impl_get_for_tuple!(A, B, C);
impl_get_for_tuple!(A, B, C, D);
impl_get_for_tuple!(A, B, C, D, E);
impl_get_for_tuple!(A, B, C, D, E, F);
impl_get_for_tuple!(A, B, C, D, E, F, G);
impl_get_for_tuple!(A, B, C, D, E, F, G, H);
impl_get_for_tuple!(A, B, C, D, E, F, G, H, I);
impl_get_for_tuple!(A, B, C, D, E, F, G, H, I, J);
