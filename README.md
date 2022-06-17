# beeasm
An assembler for the Bee16

# Building
Thanks to the beauty of rust, just do
`cargo build --release`
in the main directory to build

Alternatively, I've crated a size_opt profile for binary size optimization, which can be used by doing
`cargo build --profile size_opt`

There's also a speed optimized profile, which can be used by doing
`cargo build --profile speed_opt`

I recommend speed_opt since it's only about 100 kilobytes larger than size_opt, while release is megabytes larger than both.

# Use

(`[]` means optional)

`beeasm INPUT_FILENAME [OUTPUT_FILENAME]`

If no output filename is provided, out.bin is assumed.
