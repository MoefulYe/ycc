use miette::Result;

fn main() -> Result<()> {
    let code = r##"
// test if-else
int func(int array[]) {
  a[0] = 0xffffffffffffffffffffffffffffffffffffffffffffffffff;
  return array[3 - a[0]];
}


int main() { return (ifElse()); }
            "##;
    parser::parse("test", code)?;
    Ok(())
}
