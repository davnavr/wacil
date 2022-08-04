fn main() -> Result<(), Box<dyn std::error::Error>> {
    let current_directory = std::env::current_dir()?;
    println!("Hello, {}!", current_directory.display());
    Ok(())
}
