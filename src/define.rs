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
    ( () $family_name: ident #[cfg($unsafe_debug_cfg: meta)] ) => {
        #[cfg($unsafe_debug_cfg)]
        static IS_BORROWED: core::sync::atomic::AtomicBool =
            core::sync::atomic::AtomicBool::new(false);

        // SAFETY: cells are locked via a static `IS_BORROWED`.`
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
                        return core::result::Result::Err(
                            $crate::borrowed_error_message!("per program" $family_name),
                        );
                    }
                }

                core::result::Result::Ok(())
            }

            unsafe fn unborrow_owner() {
                #[cfg($unsafe_debug_cfg)]
                {
                    core::debug_assert!(IS_BORROWED.load(core::sync::atomic::Ordering::Relaxed));

                    IS_BORROWED.store(false, core::sync::atomic::Ordering::Release);
                }
            }
        }

        // SAFETY: `IS_BORROWED` is (un)locked in a thread-safe manner.
        unsafe impl $crate::ThreadSafeFamily for $family_name {}
        // SAFETY: `IS_BORROWED` is a static which can be accessed from any thread.
        unsafe impl $crate::SendThreadSafeFamily for $family_name {}
    };

    ( (#[can_wait]) $family_name: ident #[cfg($unsafe_debug_cfg: meta)] ) => {
        #[cfg($unsafe_debug_cfg)]
        static MUTEX: $crate::ManualMutex = $crate::ManualMutex::new();

        // SAFETY: cells are locked via a static `MUTEX`.
        unsafe impl $crate::Family for $family_name {
            const NAME: &'static str = core::stringify!($family_name);

            fn borrow_owner() -> core::result::Result<(), &'static str> {
                #[cfg($unsafe_debug_cfg)]
                {
                    if !MUTEX.try_lock() {
                        return core::result::Result::Err(
                            $crate::borrowed_error_message!("per program" $family_name),
                        );
                    }
                }

                core::result::Result::Ok(())
            }

            unsafe fn unborrow_owner() {
                #[cfg($unsafe_debug_cfg)]
                {
                    MUTEX.unlock();
                }
            }
        }

        // SAFETY: `MUTEX` is thread-safe by definition.
        unsafe impl $crate::ThreadSafeFamily for $family_name {}

        // SAFETY: `borrow_owner_or_wait()` locks the underlying `MUTEX`.
        unsafe impl $crate::WaitFamily for $family_name {
            fn borrow_owner_or_wait() {
                #[cfg($unsafe_debug_cfg)]
                {
                    MUTEX.lock()
                }
            }
        }
    };

    ( (#[can_wait(tokio)]) $family_name: ident #[cfg($unsafe_debug_cfg: meta)] ) => {
        #[cfg($unsafe_debug_cfg)]
        static MUTEX: tokio::sync::Mutex<()> = tokio::sync::Mutex::const_new(());
        #[cfg($unsafe_debug_cfg)]
        static mut MUTEX_GUARD: core::option::Option<tokio::sync::MutexGuard<'static, ()>> =
            core::option::Option::None;

        // SAFETY: cells are locked via a static `MUTEX`.
        unsafe impl $crate::Family for $family_name {
            const NAME: &'static str = core::stringify!($family_name);

            fn borrow_owner() -> core::result::Result<(), &'static str> {
                #[cfg($unsafe_debug_cfg)]
                {
                    let lock = MUTEX.try_lock()
                        .map_err(|_| $crate::borrowed_error_message!("per program" $family_name))?;

                    // SAFETY: `MUTEX_GUARD` is only accessed after successfully
                    //   locking the `MUTEX`.
                    let previous = unsafe { MUTEX_GUARD.replace(lock) };

                    core::debug_assert!(previous.is_none());
                }

                core::result::Result::Ok(())
            }

            unsafe fn unborrow_owner() {
                #[cfg($unsafe_debug_cfg)]
                {
                    // SAFETY: `unborrow_owner()` is only called by a
                    //   `CellOwner` after successfully locking the `MUTEX` in
                    //   `borrow_owner()` or `borrow_owner_or_wait_async()`.
                    let guard = MUTEX_GUARD.take();

                    core::debug_assert!(guard.is_some());
                }
            }
        }

        // SAFETY: `MUTEX` is thread-safe by definition.
        unsafe impl $crate::ThreadSafeFamily for $family_name {}

        // SAFETY: the `MUTEX_GUARD` which provides access to locked data is stored locally, so
        //   access can be sent across threads.
        unsafe impl $crate::SendThreadSafeFamily for $family_name {}

        // SAFETY: `borrow_owner_or_wait_async()` locks the underlying `MUTEX`.
        #[cfg($unsafe_debug_cfg)]
        unsafe impl $crate::AsyncWaitFamily for $family_name {
            type Future = core::pin::Pin<std::boxed::Box<
                dyn core::future::Future<Output = ()> + core::marker::Send + core::marker::Sync
            >>;

            fn borrow_owner_or_wait_async() -> Self::Future {
                Box::pin(async {
                    let lock = MUTEX.lock().await;

                    // SAFETY: `MUTEX_GUARD` is only accessed after successfully
                    //   locking the `MUTEX`.
                    let previous = unsafe { MUTEX_GUARD.replace(lock) };

                    core::debug_assert!(previous.is_none());
                })
            }
        }

        // SAFETY: unsafe, but caller used `unsafe` keyword.
        #[cfg(not($unsafe_debug_cfg))]
        unsafe impl $crate::AsyncWaitFamily for $family_name {
            type Future = core::future::Ready<()>;

            fn borrow_owner_or_wait_async() -> Self::Future {
                core::future::ready(())
            }
        }
    };

    ( (#[thread_local]) $family_name: ident #[cfg($unsafe_debug_cfg: meta)] ) => {
        $crate::if_std! {
            if std {
                #[cfg($unsafe_debug_cfg)]
                std::thread_local! {
                    static IS_BORROWED: core::cell::Cell<bool> = core::cell::Cell::new(false);
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
                static IS_BORROWED: core::cell::Cell<bool> = core::cell::Cell::new(false);
            }
        }

        // SAFETY: cells are locked via a static thread-local `IS_BORROWED`.
        unsafe impl $crate::Family for $family_name {
            const NAME: &'static str = core::stringify!($family_name);

            fn borrow_owner() -> core::result::Result<(), &'static str> {
                #[cfg(all($unsafe_debug_cfg))]
                $crate::if_std! {
                    if std {
                        #[cfg($unsafe_debug_cfg)]
                        {
                            if IS_BORROWED.with(|is_borrowed| is_borrowed.get()) {
                                return core::result::Result::Err(
                                    $crate::borrowed_error_message!("per thread" $family_name),
                                );
                            }

                            IS_BORROWED.with(|is_borrowed| is_borrowed.set(true));
                        }
                    } else {
                        #[cfg($unsafe_debug_cfg)]
                        {
                            if IS_BORROWED.get() {
                                return core::result::Result::Err(
                                    $crate::borrowed_error_message!("per thread" $family_name),
                                );
                            }

                            IS_BORROWED.set(true);
                        }
                    }
                }

                core::result::Result::Ok(())
            }

            unsafe fn unborrow_owner() {
                $crate::if_std! {
                    if std {
                        #[cfg($unsafe_debug_cfg)]
                        {
                            core::debug_assert!(IS_BORROWED.with(|is_borrowed| is_borrowed.get()));

                            IS_BORROWED.with(|is_borrowed| is_borrowed.set(false));
                        }
                    } else {
                        #[cfg($unsafe_debug_cfg)]
                        {
                            core::debug_assert!(IS_BORROWED.get());

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

/// Splits all attributes into those important to us (`can_wait`,
/// `thread_local`) and the others, then proceeds with the expansion.
#[macro_export]
#[doc(hidden)]
macro_rules! split_attrs_then_define {
    (
        #[cfg($unsafe_debug_cfg: meta)]
        ( $(#[$def: ident $($def_rest: tt)*])* ) ( $(#[$other: ident $($other_rest: tt)*])* )
        #[can_wait $($t: tt)*] $( # $rest: tt )*
        $vis: vis type $family_name: ident
    ) => {
        $crate::split_attrs_then_define! {
            #[cfg($unsafe_debug_cfg)]
            ($( #[$def $($def_rest)*] )* #[can_wait $($t)*]) ($(#[$other $($other_rest)*])*)
            $(#$rest)*
            $vis type $family_name
        }
    };
    (
        #[cfg($unsafe_debug_cfg: meta)]
        ( $(#[$def: ident $($def_rest: tt)*])* ) ( $(#[$other: ident $($other_rest: tt)*])* )
        #[thread_local $($t: tt)*] $( # $rest: tt )*
        $vis: vis type $family_name: ident
    ) => {
        $crate::split_attrs_then_define! {
            #[cfg($unsafe_debug_cfg)]
            ($( #[$def $($def_rest)*] )* #[thread_local $($t)*]) ($(#[$other $($other_rest)*])*)
            $(#$rest)*
            $vis type $family_name
        }
    };

    (
        #[cfg($unsafe_debug_cfg: meta)]
        ( $(#[$def: ident $($def_rest: tt)*])* ) ( $(#[$other: ident $($other_rest: tt)*])* )
        #[$new_other: ident $($new_other_rest: tt)*] $( # $rest: tt )*
        $vis: vis type $family_name: ident
    ) => {
        $crate::split_attrs_then_define! {
            #[cfg($unsafe_debug_cfg)]
            ($( #[$def $($def_rest)*] )*) ($(#[$other $($other_rest)*])* #[$new_other $($new_other_rest)*])
            $(#$rest)*
            $vis type $family_name
        }
    };

    (
        #[cfg($unsafe_debug_cfg: meta)]
        ( $(#[$def: ident $($def_rest: tt)*])* ) ( $(#[$other: ident $($other_rest: tt)*])* )
        $vis: vis type $family_name: ident
    ) => {
        $( #[$other $($other_rest)*] )*
        $vis enum $family_name {}

        $( #[$other $($other_rest)*] )*
        const _: () = {
            $crate::impl_family!(($(#[$def $($def_rest)*])*) $family_name #[cfg($unsafe_debug_cfg)]);
        };
    };
}

/// Defines a cell [`Family`](crate::Family), and optional [`Cell`](crate::Cell)
/// and [`CellOwner`](crate::CellOwner) aliases.
///
/// # Example
/// Define a family whose [`CellOwner`s](crate::CellOwner) are global, and
/// define the alias `BazCellOwner` for
/// [`CellOwner<BazFamily>`](crate::CellOwner):
/// ```
/// cell_family::define! {
///     /// Family for Bars.
///     pub type BazFamily: BazCellOwner;
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
/// cell_family::define! {
///     /// Family for Quuxes.
///     pub unsafe type QuuxFamily for QuuxCell<T>;
/// }
/// ```
///
/// # Example
/// Define a family whose [`CellOwner`s](crate::CellOwner) are thread-local:
/// ```
/// # #![cfg_attr(feature = "nightly", feature(thread_local))]
/// cell_family::define! {
///     /// Family for Foos.
///     #[thread_local]
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
///     #[thread_local]
///     pub type BarFamily: BarCellOwner for BarCell<B>;
/// }
/// ```
#[macro_export]
macro_rules! define {
    (
        $( #[$attr: ident $($attr_rest: tt)*] )*
        $vis: vis type $family_name: ident $(: $owner_ident: ident)? $(for $cell_ident: ident<$t: ident>)?

        $( ; $( $rest: tt )* )?
    ) => {
        $crate::split_attrs_then_define! {
            #[cfg(all())] () () $(#[$attr $($attr_rest)*])*
            $vis type $family_name
        }

        $crate::define_aliases!($vis $family_name $(: $owner_ident)? $(for $cell_ident<$t>)?);
        $( $crate::define!($($rest)*); )?
    };

    (
        $( #[$attr: ident $($attr_rest: tt)*] )*
        $vis: vis unsafe type $family_name: ident $(: $owner_ident: ident)? $(for $cell_ident: ident<$t: ident>)?

        $( ; $( $rest: tt )* )?
    ) => {
        $crate::split_attrs_then_define! {
            #[cfg(debug_assertions)] () () $(#[$attr $($attr_rest)*])*
            $vis type $family_name
        }

        $crate::define_aliases!($vis $family_name $(: $owner_ident)? $(for $cell_ident<$t>)?);
        $( $crate::define!($($rest)*); )?
    };

    () => {};
}
