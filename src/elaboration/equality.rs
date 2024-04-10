use super::*;


// TODO: add maybe equal
pub fn def_equal(ctx: &mut Context, env: &mut Environment, l: &mut Value, r: &mut Value) -> Result<bool, ElaborationError> {
    match (l, r) {
        (Value::Type, Value::Type) |
        (Value::Unit, Value::Unit) |
        (Value::Nat, Value::Nat) |
        (Value::EmptyTuple, Value::EmptyTuple) => Ok(true),

        (Value::Func(e1, p1, b1), Value::Func(e2, p2, b2)) => def_equal_func(ctx, env, e1, p1, b1, e2, p2, b2),
        (Value::BinaryTuple(l1, r1), Value::BinaryTuple(l2, r2)) => Ok(def_equal(ctx, env, l1, l2)? && def_equal(ctx, env, r1, r2)?),
        (Value::FuncType(env_l, patt_l, arg_l_type, body_l_type), Value::FuncType(env_r, patt_r, arg_r_type, body_r_type)) => {
            if !def_equal(ctx, env, &mut **arg_l_type, &mut **arg_r_type)? {
                return Ok(false);
            }

            def_equal_func(ctx, env, env_l, patt_l, body_l_type, env_r, patt_r, body_r_type)
        },
        (Value::BinaryTupleType(patt1, l1_type, r1_type), Value::BinaryTupleType(patt2, l2_type, r2_type)) => {
            if !def_equal(ctx, env, l1_type, l2_type)? {
                return Ok(false);
            }

            let g = GenericValue::new();

            env.start_scope();
            bind(ctx, env, patt1, g.into())?;
            let mut r1_type = evaluate(ctx, env, *r1_type.clone())?;
            env.end_scope();

            env.start_scope();
            bind(ctx, env, patt2, g.into())?;
            let mut r2_type = evaluate(ctx, env, *r2_type.clone())?;
            env.end_scope();

            env.start_scope();
            env.insert_binding(GenericTerm::new(), g); // FIXME: maybe unnecessary binding. will equality ever quote values?
            let res = def_equal(ctx, env, &mut r1_type, &mut r2_type)?;
            env.end_scope();
            Ok(res)
        }
        (Value::Neutral(n1), Value::Neutral(n2)) => {
            def_equal_neutral(ctx, env, n1, n2)
        },
        (Value::ExternalFunc(ext_fn), Value::ExternalFunc(ext_fn_2)) => Ok(ext_fn == ext_fn_2),
        (Value::NatNum(n1), Value::NatNum(n2)) => Ok(n1 == n2),

        (Value::BinaryTuple(..), _) |
        (Value::BinaryTupleType(..), _) |
        (Value::ExternalFunc(_), _) |
        (Value::Neutral(_), _) |
        (Value::NatNum(_), _) |
        (Value::Nat, _) |
        (Value::EmptyTuple, _) | 
        (Value::Type, _) | 
        (Value::Unit, _) | 
        (Value::Func(..), _) | 
        (Value::FuncType(..), _) => Ok(false)
    }
}

fn def_equal_func(ctx: &mut Context, env: &mut Environment, env_l: &mut Environment, patt_l: &mut Pattern, term_l: &mut Term, env_r: &mut Environment, patt_r: &mut Pattern, term_r: &mut Term) -> Result<bool, ElaborationError> {
    let g = GenericValue::new();

    env_l.start_scope();
    bind(ctx, env_l, patt_l, g.into())?;
    let mut val_l = evaluate(ctx, env_l, (*term_l).clone())?;
    env_l.end_scope();

    env_r.start_scope();
    bind(ctx, env_r, patt_r, g.into())?;
    let mut val_r = evaluate(ctx, env_r, (*term_r).clone())?;
    env_r.end_scope();
    
    env.start_scope();
    env.insert_binding(GenericTerm::new(), g); // FIXME: maybe unnecessary binding. will equality ever quote values?
    let res = def_equal(ctx, env, &mut val_l, &mut val_r)?;
    env.end_scope();
    Ok(res)
}

fn def_equal_neutral(ctx: &mut Context, env: &mut Environment, n1: &mut NeutralValue, n2: &mut NeutralValue) -> Result<bool, ElaborationError> {
    match (n1, n2) {
        (NeutralValue::Generic(g1), NeutralValue::Generic(g2)) => Ok(g1 == g2),
        (NeutralValue::Call(n1, v1), NeutralValue::Call(n2, v2)) => Ok(def_equal_neutral(ctx, env, n1, n2)? && def_equal(ctx, env, v1, v2)?),
        (NeutralValue::ExternalCall(ext_fn1, n1), NeutralValue::ExternalCall(ext_fn2, n2)) => Ok(ext_fn1 == ext_fn2 && def_equal_neutral(ctx, env, n1, n2)?),
        (NeutralValue::ExternalCall(..), _) |
        (NeutralValue::Generic(_), _) |
        (NeutralValue::Call(..), _) => Ok(false),
    }
}

#[cfg(test)]
mod tests {
    // use super::*;
    use super::{super::tests::*, super::evaluation::tests::*};

    #[test]
    fn nat_equal() {
        assert_evaluates_to_eq(
            nat(3u32), 
            nat(3u32), 
        )
    }

    #[test]
    #[should_panic]
    fn nat_unequal() {
        assert_evaluates_to_eq(
            nat(0u32), 
            nat(1u32), 
        )
    }
}