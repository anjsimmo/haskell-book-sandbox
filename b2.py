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
        match = match_beta_redex(ex, i)
        if match:
            return match

def beta_reduce_step(ex):
    match = find_leftmost_beta_redex(ex)
    if match is None:
        return # done!
    start, head_open, param, separator, body, body_close, arg, end = match
    # apply body to arg
    consumed = apply(body, param, arg)
    print ('beta:   {}([{} := {}]{}){}'.format(start, param, arg, body, end))
    ex = start + consumed + end
    return ex

def beta_reduce_all(ex):
   lim = 20
   seen = []
   step = 0
   while True:
        print ("step {}: {}".format(step, ex))
        ex = beta_reduce_step(ex)
        if ex == None:
            print("beta normal form reached")
            break
        step += 1
        if ex in seen:
            print("divergence detected")
            break
        if step > lim:
            print("failed to terminate within {} steps".format(step))
            break
        seen.append(ex)
   print('') #newline



