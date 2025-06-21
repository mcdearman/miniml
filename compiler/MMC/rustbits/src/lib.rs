pub mod token;

use std::boxed::Box;
use std::ptr;
use std::slice;

pub struct Lexer {
    logos: logos::Lexer<'static, token::TokKind>,
}

// #[no_mangle]
// pub extern "C" fn lexer_new(src_ptr: *const u8, len: usize) -> *mut c_void {
//     // Safety: caller ensures src_ptr is valid for len bytes
//     let bytes = unsafe { slice::from_raw_parts(src_ptr, len) };
//     let l = LexerWrapper {
//         inner: TokKind::lexer(bytes),
//     };
//     Box::into_raw(Box::new(l)) as *mut c_void
// }

// #[no_mangle]
// pub extern "C" fn lexer_next(lex_ptr: *mut c_void, out_tok: *mut Token) -> bool {
//     let lexer = unsafe { &mut *(lex_ptr as *mut LexerWrapper) };
//     if let Some(kind) = lexer.inner.next() {
//         let span = lexer.inner.span();
//         let t = Token {
//             kind: kind as u32,
//             start: span.start,
//             end: span.end,
//         };
//         unsafe { ptr::write(out_tok, t) };
//         true
//     } else {
//         false
//     }
// }

// #[no_mangle]
// pub extern "C" fn lexer_free(lex_ptr: *mut c_void) {
//     if !lex_ptr.is_null() {
//         unsafe {
//             Box::from_raw(lex_ptr as *mut LexerWrapper);
//         }
//     }
// }
