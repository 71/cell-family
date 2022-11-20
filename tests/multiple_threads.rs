#![cfg_attr(feature = "nightly", feature(thread_local))]

use std::sync::Arc;

cell_family::define! {
    type PerThreadFamily: PerThreadCellOwner for PerThreadCell<T>;
}

fn threads() -> usize {
    match std::thread::available_parallelism() {
        Ok(t) if t.get() > 1 => t.get(),
        _ => 32,
    }
}

#[test]
#[allow(clippy::drop_non_drop)]
fn one_owner_per_thread() {
    let owner = PerThreadCellOwner::new();
    let cell = PerThreadCell::new(1);

    std::thread::spawn(|| {
        assert!(PerThreadCellOwner::try_new().is_some());

        // We can move the cell here.
        drop(cell);
    });

    drop(owner);
}

#[test]
fn one_owner_per_program() {
    cell_family::define! {
        static type Family: CellOwner for Cell<T>;
    }

    let mut owner = CellOwner::new();
    let cell = Cell::new(vec![1, 2, 3]);

    std::thread::spawn(move || {
        // The global `owner` is still live.
        assert!(CellOwner::try_new().is_none());

        // We can move the `owner` and `cell` here (`Cell, CellOwner: Send`).
        cell.get_mut(&mut owner).push(4);

        let (owner, cell) = (Arc::new(owner), Arc::new(cell));

        std::thread::spawn(move || {
            // And reference them here (`Cell, CellOwner: Sync`).
            assert_eq!(cell.get(&owner), &[1, 2, 3, 4]);
        });
    });
}

#[test]
fn cannot_race() {
    cell_family::define! {
        static type Family: CellOwner for Cell<T>;
    }

    let threads = threads();
    let cell_owners = Arc::new(std::sync::RwLock::new(0));

    // Spawn threads simultaneously, make them all access the `CellOwner`, and
    // make sure that only one had access.
    let handles = (0..threads)
        .map(|_| {
            let cell_owners = cell_owners.clone();

            std::thread::spawn(move || {
                let cell_owner = CellOwner::try_new();

                if cell_owner.is_some() {
                    *cell_owners.write().unwrap() += 1;
                }

                std::thread::sleep(std::time::Duration::from_millis(500));

                drop(cell_owner);
            })
        })
        .collect::<Vec<_>>();

    for handle in handles {
        assert!(handle.join().is_ok());
    }

    assert_ne!(*cell_owners.read().unwrap(), 0);
    assert!(CellOwner::try_new().is_some());
}

#[test]
fn cannot_race_after_drop() {
    cell_family::define! {
        static type Family: CellOwner for Cell<T>;
    }

    let threads = threads();
    let cell_owners = Arc::new(std::sync::RwLock::new(0));
    let cell_owner = CellOwner::new();

    let handles = (0..threads)
        .map(|_| {
            let cell_owners = cell_owners.clone();

            std::thread::spawn(move || {
                let cell_owner = CellOwner::try_new();

                if cell_owner.is_some() {
                    *cell_owners.write().unwrap() += 1;
                }

                std::thread::sleep(std::time::Duration::from_millis(200));

                drop(cell_owner);
            })
        })
        .collect::<Vec<_>>();

    drop(cell_owner);

    for handle in handles {
        assert!(handle.join().is_ok());
    }

    assert_ne!(*cell_owners.read().unwrap(), 0);
    assert!(CellOwner::try_new().is_some());
}

#[test]
fn cannot_race_cell() {
    cell_family::define! {
        static type Family: CellOwner for Cell<T>;
    }

    let threads = threads();
    let seen = Arc::new(Cell::new(0));

    let handles = (0..threads)
        .map(|_| {
            let seen = seen.clone();

            std::thread::spawn(move || {
                if let Some(mut owner) = CellOwner::try_new() {
                    *seen.get_mut(&mut owner) += 1;
                }
            })
        })
        .collect::<Vec<_>>();

    for handle in handles {
        assert!(handle.join().is_ok());
    }

    let owner = CellOwner::new();

    assert_ne!(*seen.get(&owner), 0);
}
