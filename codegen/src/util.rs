use rand::{distributions::Alphanumeric, thread_rng, Rng};

pub fn prefix(ident: &str) -> String {
    let mut ret = thread_rng()
        .sample_iter(&Alphanumeric)
        .take(32)
        .map(char::from)
        .fold("_".to_owned(), |mut acc, ch| {
            acc.push(ch);
            acc
        });
    ret.push_str("_");
    ret.push_str(ident);
    ret
}
