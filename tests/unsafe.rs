#![cfg_attr(feature = "nightly", feature(thread_local))]

macro_rules! test_all {
    (
        type $family_name: ident: $owner_name: ident for $cell_name: ident;
        type $other_family_name: ident: $other_owner_name: ident for $other_cell_name: ident;

        $(
            $( #[$attr: meta] )*
            fn $test_name: ident() { $($test_body: tt)* }
        )+
    ) => {
        $(
            // Since tests run in parallel, we create new families for each test.
            $( #[$attr] )*
            fn $test_name() {
                cell_family::define!(type $family_name: $owner_name for $cell_name<T>);
                cell_family::define!(unsafe type $other_family_name: $other_owner_name for $other_cell_name<T>);

                $( $test_body )*
            }
        )+

        mod thread_local_tests {
            cell_family::define!(#[thread_local] type $family_name: $owner_name for $cell_name<T>);
            cell_family::define!(#[thread_local] unsafe type $other_family_name: $other_owner_name for $other_cell_name<T>);

            $(
                $( #[$attr] )*
                fn $test_name() {
                    $( $test_body )*
                }
            )+
        }
    };
}

test_all! {
    type Family: CellOwner for Cell;
    type UnsafeFamily: UnsafeCellOwner for UnsafeCell;

    #[test]
    fn basic() {
        let mut owner = CellOwner::new();
        let a = Cell::new(1);
        let b = Cell::new("world");

        assert!(owner.try_get_mut((&a, &b)).is_some());
        assert!(owner.try_get_mut((&a, &a)).is_none());

        assert!(CellOwner::try_new().is_none());

        drop(owner);

        assert!(CellOwner::try_new().is_some());
    }
}
