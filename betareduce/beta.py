#!/usr/bin/env python
import string

LAM = "\\" # lambda represented with single back slash

def match_beta_redex(ex, i=0):
    # Search for expression of form (\x.y)(z)

    start = ex[:i]

    # (\...
    LAM_OPEN = "(" + LAM
    if not ex[i:i+2] == LAM_OPEN:
        return

    i += 2
    paren_depth = 1

    head_open = LAM_OPEN
    
    param = ""
    while True:
        # if ex starts with a lambda, then it should have an end
        assert not i >= len(ex)
        char = ex[i]
        # the head should just contain a single variable
        assert char not in "()" + LAM, "{} {}".format(i, char)
        if char == ".":
            # found the body
            break
        param += char
        i += 1

    separator = "."
    i += 1

    body = ""
    while True:
        # if ex has a body, then it should have an end
        assert not i >= len(ex)
        char = ex[i]
        if char == "(":
            paren_depth += 1
        elif char == ")":
            paren_depth -= 1
        if paren_depth == 0:
            # found the end of the body
            break

        body += char
        i += 1

    body_close = ")"
    i += 1

    arg = ""
    while True:
        # its possible that the ex ends without any args
        if i >= len(ex):
            # there is no excuse for the brackets not being balanced
            assert paren_depth == 0
            return None
        char = ex[i]
        if char == "(":
            paren_depth += 1
        elif char == ")":
            paren_depth -= 1
        
        arg += char
        i += 1

        if paren_depth == 0:
            break
        if paren_depth < 0:
            # it's possible that the lambda is nested without any args
            return None

    end = ex[i:]

    return start, head_open, param, separator, body, body_close, arg, end

def alpha_equiv(ex, oldvar, newvar, two_way=True):
    if not two_way:
       assert not newvar in ex
    
    # swap oldvar and newvar
    tmp = "T"
    ex = ex.replace(newvar, tmp)
    ex = ex.replace(oldvar, newvar)
    ex = ex.replace(tmp, oldvar)
    return ex

def apply(ex, param, val):
    ex = ex.replace(param, val)
    return ex

def find_leftmost_beta_redex(ex):
    for i in range(len(ex)):
        match = match_beta_redex(ex, i)
        if match:
            return match

def gen_new_vars(var_names, banned):
    allowed = set(string.ascii_lowercase) - set(banned) - set(var_names)
    if len(allowed) < len(var_names):
        raise Exception("Ran out of alphabetic variable names!")
    new_names = sorted(allowed)[:len(var_names)]
    return new_names

def extract_params(ex):
    """
    Returns list of var names that are parameters within head of any lambda expression
    """
    bound_vars = set()
    in_head = False
    for char in ex:
        if char == LAM:
            in_head = True
        elif char == ".":
            in_head = False
        if in_head:
            if char in string.ascii_lowercase:
                bound_vars.add(char)
    return bound_vars

def reduce_step(ex):
    match = find_leftmost_beta_redex(ex)
    if match is None:
        return # done!
    start, head_open, param, separator, body, body_close, arg, end = match
    lam_term = head_open + param + separator + body + body_close
    # free vars are not in conflict, because lam_term and arg share the same binding for free vars
    arg_params = extract_params(arg)
    lam_params = extract_params(lam_term)
    used_vars  = extract_vars(arg) | extract_vars(lam_params) # avoid any var in use, whether free or param
    conflicts = sorted(arg_params & lam_params)
    if conflicts:
        new_vars = gen_new_vars(conflicts, used_vars)
        renames = list(zip(conflicts, new_vars))
        rename_str = ', '.join(['{}<->{}'.format(r[0], r[1]) for r in renames])
        print ('alpha:  {}([{}]\\{}){}'.format(start, rename_str, param + separator + body, arg + end))
        for old, new in renames:
            lam_term = alpha_equiv(lam_term, old, new, two_way=False)
        ex = start + lam_term + arg + end
        return ex
    else:
        # apply body to arg
        consumed = apply(body, param, arg)
        print ('beta:   {}([{} := {}]{}){}'.format(start, param, arg, body, end))
        ex = start + consumed + end
        return ex

def extract_vars(ex):
    """return set of all var names appearing anywhere within ex"""
    return set(ex) & set(string.ascii_lowercase)

def beta_reduce(ex, lim=20):
   seen = []
   step = 0
   solved = False
   while True:
        print ("step {}: {}".format(step, ex))
        _ex = reduce_step(ex)
        if _ex == None:
            print("beta normal form reached")
            solved = True
            break
        step += 1
        if _ex in seen:
            print("divergence detected")
            break
        if step > lim:
            print("failed to terminate within {} steps".format(step))
            break
        seen.append(_ex)
        ex = _ex
   print('') #newline
   if solved:
       return ex
   else:
       return None

