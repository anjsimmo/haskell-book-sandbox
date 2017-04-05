#!/usr/bin/env python

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
        char = ex[i]
        # if ex starts with a lambda, then it should have an end
        assert char is not None
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
        # if ex starts with a lambda, then it should have an end
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
        char = ex[i]
        if char == "(":
            paren_depth += 1
        elif char == ")":
            paren_depth -= 1
        
        arg += char
        i += 1

        if paren_depth == 0:
            break

    end = ex[i:]

    return start, head_open, param, separator, body, body_close, arg, end

def alpha_equiv(ex, oldvar, newvar):
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
        _ex = ex[i:] 
        match = match_beta_redex(_ex)
        if match:
            return match

def beta_reduce_step(ex):
    match = find_leftmost_beta_redex(ex)
    if match is None:
        print ("Done!")
        return
    start, head_open, param, separator, body, body_close, arg, end = match
    # apply body to arg
    consumed = apply(body, param, arg)
    ex = start + consumed + end
    return ex

