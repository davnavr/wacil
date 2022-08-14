import "wasi"

export function hello(): void {
    console.log("Hello WASI! ")
    for (let i = 0; i < process.argv.length; ++i) {
        console.log(process.argv[i]);
    }
}
