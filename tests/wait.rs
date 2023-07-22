use std::{sync::Arc, time::Duration};

macro_rules! define_cells {
    () => {
        cell_family::define! {
            #[can_wait] type WaitFamily: WaitOwner for WaitCell<T>;
            #[can_wait(tokio)] type AsyncWaitFamily: AsyncWaitOwner for AsyncWaitCell<T>;
        }
    };
}

#[test]
fn can_wait() {
    define_cells!();

    let data = Arc::new(WaitCell::new(0));
    let data_clone = data.clone();
    let mut owner = WaitOwner::new();

    std::thread::spawn(move || {
        assert!(WaitOwner::try_new().is_none());

        let mut owner = WaitOwner::wait();

        assert_eq!(*data_clone.get(&owner), 1);
        *data_clone.get_mut(&mut owner) = 2;
    });

    std::thread::sleep(Duration::from_millis(40));

    assert_eq!(*data.get(&owner), 0);
    *data.get_mut(&mut owner) = 1;

    drop(owner);

    std::thread::sleep(Duration::from_millis(10));

    let owner = WaitOwner::wait();

    assert_eq!(*data.get(&owner), 2);
}

#[tokio::test]
async fn can_wait_async() {
    define_cells!();

    let data = Arc::new(AsyncWaitCell::new(0));
    let data_clone = data.clone();
    let mut owner = AsyncWaitOwner::new();

    tokio::spawn(async move {
        assert!(AsyncWaitOwner::try_new().is_none());

        let mut owner = AsyncWaitOwner::wait_async().await;

        assert_eq!(*data_clone.get(&owner), 1);
        *data_clone.get_mut(&mut owner) = 2;
    });

    tokio::time::sleep(Duration::from_millis(40)).await;

    assert_eq!(*data.get(&owner), 0);
    *data.get_mut(&mut owner) = 1;

    drop(owner);

    let owner = AsyncWaitOwner::wait_async().await;

    assert_eq!(*data.get(&owner), 2);
}

#[tokio::test]
async fn can_select() {
    define_cells!();

    let owner = AsyncWaitOwner::new();

    tokio::spawn(async move {
        // Ensure that the `owner` cannot be accessed immediately.
        tokio::time::sleep(Duration::from_millis(40)).await;

        drop(owner);
    });

    tokio::select! {
        _ = AsyncWaitOwner::wait_async() => {
            assert!(false, "AsyncWaitOwner obtained within 10ms");
        }
        _ = tokio::time::sleep(Duration::from_millis(10)) => {}
    }

    tokio::select! {
        _ = AsyncWaitOwner::wait_async() => {

        }
        _ = tokio::time::sleep(Duration::from_millis(50)) => {
            assert!(false, "AsyncWaitOwner not obtained within 50ms");
        }
    }
}
