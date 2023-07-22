#![cfg_attr(feature = "nightly", feature(thread_local))]

macro_rules! test_all {
    (
        type $family_name: ident: $owner_name: ident for $cell_name: ident;
        type $other_family_name: ident;

        $(
            $( #[$attr: meta] )*
            fn $test_name: ident() { $($test_body: tt)* }
        )+
    ) => {
        $(
            // Since tests run in parallel, we create new families for each test.
            $( #[$attr] )*
            fn $test_name() {
                cell_family::define! {
                    type $family_name: $owner_name for $cell_name<T>;
                    type $other_family_name;
                }

                $( $test_body )*
            }
        )+

        mod thread_local_tests {
            $(
                $( #[$attr] )*
                fn $test_name() {
                    cell_family::define! {
                        #[thread_local] type $family_name: $owner_name for $cell_name<T>;
                        #[thread_local] type $other_family_name;
                    }

                    $( $test_body )*
                }
            )+
        }

        #[cfg(feature = "std")]
        mod wait_tests {
            $(
                $( #[$attr] )*
                fn $test_name() {
                    cell_family::define! {
                        #[can_wait] type $family_name: $owner_name for $cell_name<T>;
                        #[can_wait] type $other_family_name;
                    }

                    $( $test_body )*
                }
            )+
        }
    };
}

test_all! {
    type Family: CellOwner for Cell;
    type OtherFamily;

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

    #[test]
    fn multiple_borrows() {
        let owner = CellOwner::new();
        let a = Cell::new(1);
        let b = Cell::new("1");

        assert_eq!(b.get(&owner).parse(), Ok(*a.get(&owner)));
    }

    #[test]
    fn different_families() {
        let mut owner = CellOwner::new();
        let mut other_owner = cell_family::CellOwner::<OtherFamily>::new();

        let value = Cell::new(1);
        let other_value = cell_family::Cell::<OtherFamily, _>::new(2);

        assert_eq!(*value.get_mut(&mut owner) + 1, *other_value.get_mut(&mut other_owner));
    }
}
