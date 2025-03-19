use syn::{Path, visit_mut::VisitMut};

pub struct RewritePaths {
    pub(super) crate_root_path: Path,
    pub(super) module_path: Option<Path>,
}

impl VisitMut for RewritePaths {
    fn visit_path_mut(&mut self, path: &mut Path) {
        if path.segments.first().is_some_and(|s| s.ident == "crate") {
            let path_tail = path.segments.iter().skip(1);
            path.segments = self
                .crate_root_path
                .segments
                .iter()
                .chain(path_tail)
                .cloned()
                .collect();
        } else if let Some(module_path) = &self.module_path {
            let mut path_skip_count = 0;
            let mut module_skip_end_count = 0;

            for segment in path.segments.iter() {
                if segment.ident == "self" {
                    path_skip_count += 1;
                } else if segment.ident == "super" {
                    path_skip_count += 1;
                    module_skip_end_count += 1;
                } else {
                    break;
                }
            }

            if path_skip_count > 0 {
                let path_tail = path.segments.iter().skip(path_skip_count);
                path.segments = module_path
                    .segments
                    .iter()
                    .take(module_path.segments.len() - module_skip_end_count)
                    .chain(path_tail)
                    .cloned()
                    .collect();
            }
        }
    }
}
