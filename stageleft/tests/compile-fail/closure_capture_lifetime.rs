#![allow(unexpected_cfgs)]
use stageleft::*;

fn closure_capture_lifetime_2<'a, I: Copy + Into<u32>>(
    _ctx: BorrowBounds<'a>,
    v: RuntimeData<I>,
) -> impl Quoted<'a, Box<dyn Fn() -> u32 + 'a>> {
    q!(Box::new(move || { v.into() }) as Box<dyn Fn() -> u32 + 'a>)
}

fn main() {}
