use rand::{distributions::Alphanumeric, thread_rng, Rng};

pub fn prefix(func: &str, ident: &str) -> String {
    let mut ret = thread_rng()
        .sample_iter(&Alphanumeric)
        .take(32)
        .map(char::from)
        .fold(
            {
                let mut s = func.to_owned();
                s.push_str("::");
                s
            },
            |mut acc, ch| {
                acc.push(ch);
                acc
            },
        );
    ret.push_str("::");
    ret.push_str(ident);
    ret
}
