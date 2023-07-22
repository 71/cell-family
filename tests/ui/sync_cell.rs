cell_family::define! {
    #[thread_local]
    type Family: CellOwner for Cell<T>;
}

fn main() {
    let cell = Cell::new(vec![1, 2, 3]);

    std::thread::spawn(|| {
        // We *cannot* reference the `cell` here (`Cell: !Sync`).
        let mut owner = CellOwner::new();

        cell.get_mut(&mut owner).push(4);
    });
}
