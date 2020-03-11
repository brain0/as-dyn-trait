use syn::{
    fold::{fold_generics, fold_lifetime_def, fold_type_param, Fold},
    ConstParam, Generics, LifetimeDef, TypeParam,
};

struct RemoveDefaults;

impl Fold for RemoveDefaults {
    fn fold_const_param(&mut self, _i: ConstParam) -> ConstParam {
        todo!("const generics are not yet supported")
    }

    fn fold_type_param(&mut self, i: TypeParam) -> TypeParam {
        TypeParam {
            eq_token: None,
            default: None,
            ..fold_type_param(self, i)
        }
    }
}

struct RemoveBounds;

impl Fold for RemoveBounds {
    fn fold_const_param(&mut self, _i: ConstParam) -> ConstParam {
        todo!("const generics are not yet supported")
    }

    fn fold_generics(&mut self, i: Generics) -> Generics {
        Generics {
            where_clause: None,
            ..fold_generics(self, i)
        }
    }

    fn fold_lifetime_def(&mut self, i: LifetimeDef) -> LifetimeDef {
        LifetimeDef {
            colon_token: None,
            bounds: Default::default(),
            ..fold_lifetime_def(self, i)
        }
    }

    fn fold_type_param(&mut self, i: TypeParam) -> TypeParam {
        TypeParam {
            colon_token: None,
            bounds: Default::default(),
            eq_token: None,
            default: None,
            ..fold_type_param(self, i)
        }
    }
}

pub(crate) trait GenericsExt {
    fn remove_defaults(self) -> Self;
    fn remove_bounds(self) -> Self;
}

impl GenericsExt for Generics {
    fn remove_defaults(self) -> Self {
        RemoveDefaults.fold_generics(self)
    }

    fn remove_bounds(self) -> Self {
        RemoveBounds.fold_generics(self)
    }
}
