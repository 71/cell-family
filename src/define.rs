#[macro_export]
#[doc(hidden)]
macro_rules! crate_name {
    () => {
        "cell_family"
    };
}

#[macro_export]
#[doc(hidden)]
macro_rules! borrowed_error_message {
    ( $per: literal $family_name: ident ) => {
        core::concat!(
            "Only one instance of `",
            $crate::crate_name!(),
            "::CellOwner<",
            core::module_path!(),
            "::",
            core::stringify!($family_name),
            ">` can exist ",
            $per,
            " at a time",
        )
    };
}

#[cfg(feature = "std")]
#[macro_export]
#[doc(hidden)]
macro_rules! if_std {
    ( if std { $($then: tt)* } else { $($else: tt)* } ) => { $($then)* };
}

#[cfg(not(feature = "std"))]
#[macro_export]
#[doc(hidden)]
macro_rules! if_std {
    ( if std { $($then: tt)* } else { $($else: tt)* } ) => { $($else)* };
}

#[macro_export]
#[doc(hidden)]
macro_rules! impl_family {
    ( static $family_name: ident #[cfg($unsafe_debug_cfg: meta)] ) => {
        #[cfg($unsafe_debug_cfg)]
        static IS_BORROWED: core::sync::atomic::AtomicBool =
            core::sync::atomic::AtomicBool::new(false);

        unsafe impl $crate::Family for $family_name {
            const NAME: &'static str = core::stringify!($family_name);

            fn borrow_owner() -> core::result::Result<(), &'static str> {
                #[cfg($unsafe_debug_cfg)]
                {
                    if IS_BORROWED.compare_exchange(
                        false,
                        true,
                        core::sync::atomic::Ordering::Acquire,  // TODO: is this right?
                        core::sync::atomic::Ordering::Acquire,  // TODO: is this right?
                    ).is_err() {
                        return Err($crate::borrowed_error_message!("per program" $family_name));
                    }
                }

                Ok(())
            }

            fn unborrow_owner() {
                #[cfg($unsafe_debug_cfg)]
                {
                    debug_assert!(IS_BORROWED.load(core::sync::atomic::Ordering::Relaxed));

                    IS_BORROWED.store(false, core::sync::atomic::Ordering::Release);
                }
            }
        }

        unsafe impl $crate::ThreadSafeFamily for $family_name {}
    };

    ( $family_name: ident #[cfg($unsafe_debug_cfg: meta)] ) => {
        $crate::if_std! {
            if std {
                #[cfg($unsafe_debug_cfg)]
                std::thread_local! {
                    static IS_BORROWED: std::cell::Cell<bool> = std::cell::Cell::new(false);
                }
            } else {
                #[$crate::rustversion::not(nightly)]
                const _: () = {
                    core::compile_error!(
                        "To define a thread-local `Family` either the `std` feature must be enabled, or Rust nightly must be used."
                    )
                };

                #[$crate::rustversion::nightly]
                #[cfg($unsafe_debug_cfg)]
                #[thread_local]
                static IS_BORROWED: std::cell::Cell<bool> = std::cell::Cell::new(false);
            }
        }

        unsafe impl $crate::Family for $family_name {
            const NAME: &'static str = core::stringify!($family_name);

            fn borrow_owner() -> core::result::Result<(), &'static str> {
                #[cfg(all($unsafe_debug_cfg))]
                $crate::if_std! {
                    if std {
                        #[cfg($unsafe_debug_cfg)]
                        {
                            if IS_BORROWED.with(|is_borrowed| is_borrowed.get()) {
                                return Err(
                                    $crate::borrowed_error_message!("per thread" $family_name),
                                );
                            }

                            IS_BORROWED.with(|is_borrowed| is_borrowed.set(true));
                        }
                    } else {
                        #[cfg($unsafe_debug_cfg)]
                        {
                            if IS_BORROWED.get() {
                                return Err(
                                    $crate::borrowed_error_message!("per thread" $family_name),
                                );
                            }

                            IS_BORROWED.set(true);
                        }
                    }
                }

                Ok(())
            }

            fn unborrow_owner() {
                $crate::if_std! {
                    if std {
                        #[cfg($unsafe_debug_cfg)]
                        {
                            debug_assert!(IS_BORROWED.with(|is_borrowed| is_borrowed.get()));

                            IS_BORROWED.with(|is_borrowed| is_borrowed.set(false));
                        }
                    } else {
                        #[cfg($unsafe_debug_cfg)]
                        {
                            debug_assert!(IS_BORROWED.get());

                            IS_BORROWED.set(false);
                        }
                    }
                }
            }
        }
    };
}

#[macro_export]
#[doc(hidden)]
macro_rules! define_aliases {
    ( $vis: vis $family_name: ident: $owner_ident: ident for $cell_ident: ident <$t: ident> ) => {
        #[doc = core::concat!(
            "[Owner](",
            $crate::crate_name!(),
            "::CellOwner) of a [`",
            core::module_path!(),
            "::",
            core::stringify!($cell_ident),
            "`]."
        )]
        $vis type $owner_ident = $crate::CellOwner<$family_name>;

        #[doc = core::concat!(
            "[Cell](",
            $crate::crate_name!(),
            "::Cell) owned by a [`",
            core::module_path!(),
            "::",
            core::stringify!($owner_ident),
            "`]."
        )]
        $vis type $cell_ident<$t> = $crate::Cell<$family_name, $t>;
    };

    ( $vis: vis $family_name: ident: $owner_ident: ident ) => {
        #[doc = core::concat!(
            "[Owner](",
            $crate::crate_name!(),
            "::CellOwner) of a [`",
            $crate::crate_name!(),
            "::Cell<",
            core::module_path!(),
            "::",
            core::stringify!($family_name),
            ">`]."
        )]
        $vis type $owner_ident = $crate::CellOwner<$family_name>;
    };

    ( $vis: vis $family_name: ident for $cell_ident: ident <$t: ident> ) => {
        #[doc = core::concat!(
            "[Cell](",
            $crate::crate_name!(),
            "::Cell) owned by a [`",
            $crate::crate_name!(),
            "::Cell<",
            core::module_path!(),
            "::",
            core::stringify!($family_name),
            ">`]."
        )]
        $vis type $cell_ident<$t> = $crate::Cell<$family_name, $t>;
    };

    ( $vis: vis $family_name: ident ) => {};
}

/// Defines a cell [`Family`](crate::Family), and optional [`Cell`](crate::Cell)
/// and [`CellOwner`](crate::CellOwner) aliases.
///
/// # Example
/// Define a family whose [`CellOwner`s](crate::CellOwner) are thread-local:
/// ```
/// # #![cfg_attr(feature = "nightly", feature(thread_local))]
/// cell_family::define! {
///     /// Family for Foos.
///     pub type FooFamily;
/// }
/// ```
///
/// # Example
/// Define a family whose [`CellOwner`s](crate::CellOwner) are thread-local, and
/// define the alias `BarCellOwner` for
/// [`CellOwner<BarFamily>`](crate::CellOwner) and `BarCell<B>` for
/// [`Cell<BarFamily, B>`](crate::Cell):
/// ```
/// # #![cfg_attr(feature = "nightly", feature(thread_local))]
/// cell_family::define! {
///     /// Family for Bars.
///     pub type BarFamily: BarCellOwner for BarCell<B>;
/// }
/// ```
///
/// # Example
/// Define a family whose [`CellOwner`s](crate::CellOwner) are global, and
/// define the alias `BazCellOwner` for
/// [`CellOwner<BazFamily>`](crate::CellOwner):
/// ```
/// # #![cfg_attr(feature = "nightly", feature(thread_local))]
/// cell_family::define! {
///     /// Family for Bars.
///     pub static type BazFamily: BazCellOwner;
/// }
/// ```
///
/// # Example
/// Define a family whose [`CellOwner`s](crate::CellOwner) are global, and
/// define the alias `QuuxCell<T>` for [`Cell<QuuxFamily, T>`](crate::Cell); in
/// release builds (`cfg!(debug_assertions)` is `false`), **all checks** are
/// disabled, and the cell behaves like an
/// [`UnsafeCell<T>`](core::cell::UnsafeCell):
/// ```
/// # #![cfg_attr(feature = "nightly", feature(thread_local))]
/// cell_family::define! {
///     /// Family for Quuxes.
///     pub unsafe static type QuuxFamily for QuuxCell<T>;
/// }
/// ```
#[macro_export]
macro_rules! define {
    (
        $( #[$attr: meta] )*
        $vis: vis static type $family_name: ident $(: $owner_ident: ident)? $(for $cell_ident: ident<$t: ident>)?

        $( ; $( $rest: tt )* )?
    ) => {
        $( #[$attr] )*
        $vis enum $family_name {}

        const _: () = {
            $crate::impl_family!(static $family_name #[cfg(all())]);
        };

        $crate::define_aliases!($vis $family_name $(: $owner_ident)? $(for $cell_ident<$t>)?);
        $( $crate::define!($($rest)*); )?
    };

    (
        $( #[$attr: meta] )*
        $vis: vis unsafe static type $family_name: ident $(: $owner_ident: ident)? $(for $cell_ident: ident<$t: ident>)?

        $( ; $( $rest: tt )* )?
    ) => {
        $( #[$attr] )*
        $vis enum $family_name {}

        const _: () = {
            $crate::impl_family!(static $family_name #[cfg(debug_assertions)]);
        };

        $crate::define_aliases!($vis $family_name $(: $owner_ident)? $(for $cell_ident<$t>)?);
        $( $crate::define!($($rest)*); )?
    };

    (
        $( #[$attr: meta] )*
        $vis: vis type $family_name: ident $(: $owner_ident: ident)? $(for $cell_ident: ident<$t: ident>)?

        $( ; $( $rest: tt )* )?
    ) => {
        $( #[$attr] )*
        $vis enum $family_name {}

        const _: () = {
            $crate::impl_family!($family_name #[cfg(all())]);
        };

        $crate::define_aliases!($vis $family_name $(: $owner_ident)? $(for $cell_ident<$t>)?);
        $( $crate::define!($($rest)*); )?
    };

    (
        $( #[$attr: meta] )*
        $vis: vis unsafe type $family_name: ident $(: $owner_ident: ident)? $(for $cell_ident: ident<$t: ident>)?

        $( ; $( $rest: tt )* )?
    ) => {
        $( #[$attr] )*
        $vis enum $family_name {}

        const _: () = {
            $crate::impl_family!($family_name #[cfg(debug_assertions)]);
        };

        $crate::define_aliases!($vis $family_name $(: $owner_ident)? $(for $cell_ident<$t>)?);
        $( $crate::define!($($rest)*); )?
    };

    () => {};
}
