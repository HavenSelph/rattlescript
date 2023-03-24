macro_rules! error {
    ($($arg:tt)*) => {
        {
            let x = format!($($arg)*);
            println!("{}", String::from("─").repeat(x.len()));
            println!("{}", x);
            println!("{}", String::from("─").repeat(x.len()));
            panic!();
        }
    }
}

pub(crate) use error;
