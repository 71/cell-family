cell_family::define! {
    type Family: CellOwner for Cell<T>;
}

fn main() {
    let mut owner = CellOwner::new();

    std::thread::spawn(move || {
        // We *cannot* move the `owner` here (`CellOwner: !Send`).
        let cell = Cell::new(vec![1, 2, 3]);

        cell.get_mut(&mut owner).push(4);
    });
}
