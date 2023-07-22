cell_family::define! {
    #[thread_local]
    type Family: CellOwner for Cell<T>;
}

fn main() {
    let mut owner = CellOwner::new();

    std::thread::spawn(|| {
        // We *cannot* reference the `owner` here (`CellOwner: !Sync`).
        let cell = Cell::new(vec![1, 2, 3]);

        cell.get_mut(&mut owner).push(4);
    });
}
