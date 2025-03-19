use syn::{Path, visit_mut::VisitMut};

pub struct RewritePaths {
    pub(super) crate_root_path: Path,
    pub(super) module_path: Option<Path>,
}

impl VisitMut for RewritePaths {
    fn visit_path_mut(&mut self, path: &mut Path) {
        if path
            .segments
            .first()
            .map(|s| s.ident == "crate")
            .unwrap_or(false)
        {
            let path_tail = path.segments.iter().skip(1);
            path.segments = self
                .crate_root_path
                .segments
                .iter()
                .chain(path_tail)
                .cloned()
                .collect();
        } else if let Some(module_path) = &self.module_path {
            if path
                .segments
                .first()
                .map(|s| s.ident == "self")
                .unwrap_or(false)
            {
                let path_tail = path.segments.iter().skip(1);
                path.segments = module_path
                    .segments
                    .iter()
                    .chain(path_tail)
                    .cloned()
                    .collect();
            } else if path
                .segments
                .first()
                .map(|s| s.ident == "super")
                .unwrap_or(false)
            {
                let super_count = path
                    .segments
                    .iter()
                    .take_while(|s| s.ident == "super")
                    .count();

                let path_tail = path.segments.iter().skip(super_count);
                path.segments = module_path
                    .segments
                    .iter()
                    .take(module_path.segments.len() - super_count)
                    .chain(path_tail)
                    .cloned()
                    .collect();
            }
        }
    }
}
