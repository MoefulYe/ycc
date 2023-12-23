use rand::{distributions::Alphanumeric, thread_rng, Rng};

pub fn id() -> String {
    thread_rng()
        .sample_iter(&Alphanumeric)
        .take(32)
        .map(char::from)
        .fold("id_".to_owned(), |mut acc, ch| {
            acc.push(ch);
            acc
        })
}
