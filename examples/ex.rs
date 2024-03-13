

fn main() {
    let slice = ['r', 'u', 's', 't'];
    let iter = slice.windows(2).collect::<Vec<_>>();
    dbg!(iter);
}