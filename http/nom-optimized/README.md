compilation line to activate SSE4.2:

`RUSTFLAGS="-C target-feature=+sse2" cargo build --release`

Without the flags`(or if your CPU does not support it), you might get
the following compilation error:

```
LLVM ERROR: Cannot select: 0x111a21548: i32,i32 = X86ISD::PCMPESTRI 0x111a219c0, 0x111f2d138, 0x111f2d478, 0x111f2d3a8, Constant:i8<-2>
  0x111a219c0: v16i8,ch = CopyFromReg 0x1114eb040, Register:v16i8 %254
    0x111a21f70: v16i8 = Register %254
  0x111f2d138: i32,ch = CopyFromReg 0x1114eb040, Register:i32 %255
    0x111a213a8: i32 = Register %255
  0x111f2d478: v16i8,ch = CopyFromReg 0x1114eb040, Register:v16i8 %256
    0x111a21000: v16i8 = Register %256
  0x111f2d3a8: i32,ch = CopyFromReg 0x1114eb040, Register:i32 %257
    0x111f2d270: i32 = Register %257
  0x111f2d548: i8 = Constant<-2>
In function: _ZN8nom_http12request_line17hb6e90fc25fb41b61E
```

to build without simd (replacing with loop unrolling):

`cargo build --release --no--default-features`
