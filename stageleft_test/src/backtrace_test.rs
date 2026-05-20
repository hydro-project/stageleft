use stageleft::{BorrowBounds, Quoted, q};

#[stageleft::entry]
pub fn panicking_entry(_ctx: BorrowBounds<'_>) -> impl Quoted<'_, i32> {
    q!(panic!("q site panic"))
}

#[cfg(stageleft_runtime)]
#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_panic_span_attribution() {
        use std::sync::{Arc, Mutex};
        let bt_store: Arc<Mutex<Option<String>>> = Arc::new(Mutex::new(None));
        let bt_clone = bt_store.clone();
        let prev_hook = std::panic::take_hook();
        std::panic::set_hook(Box::new(move |_| {
            let bt = std::backtrace::Backtrace::force_capture();
            *bt_clone.lock().unwrap() = Some(bt.to_string());
        }));
        let result = std::panic::catch_unwind(|| {
            panicking_entry!();
        });
        std::panic::set_hook(prev_hook);
        assert!(result.is_err());
        let bt_str = bt_store.lock().unwrap().take().unwrap();
        // The backtrace should contain a frame pointing to the q!() definition site
        // (line 5 where `q!(panic!("q site panic"))` is written).
        assert!(
            bt_str.contains("backtrace_test.rs:5"),
            "Backtrace should contain backtrace_test.rs:5 (the q! site), got:\n{bt_str}"
        );
    }
}
