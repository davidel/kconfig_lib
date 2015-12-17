#!/usr/bin/python
# Local Variables:
# mode: python
# End:

import os
import re
import glob


TOKEN_EOF = 0
TOKEN_LNOT = 1
TOKEN_NAME = 2
TOKEN_OPAR = 3
TOKEN_CPAR = 4
TOKEN_LOR = 5
TOKEN_LAND = 6
TOKEN_EQ = 7
TOKEN_NEQ = 8
TOKEN_LITERAL = 9
TOKEN_IF = 10
TOKEN_VALUE = 11

remap_dict = {'m': 'MODULES'}

dbg_level = 10


class depend_chain:
    def __init__(self, next):
        self.next = next
        self.depends = []

    def get_next(self):
        return self.next

    def set_next(self, next):
        self.next = next

    def get_depends(self):
        return self.depends

    def add_depends(self, dep):
        self.depends.append(dep)


class loc_op:
    def __init__(self, op = 'NONE', fname = 'GENERATED', line = -1):
        self.op = op
        self.fname = fname
        self.line = line

    def get_op(self):
        return self.op

    def get_fname(self):
        return self.fname

    def get_line(self):
        return self.line


class tree_entry:
    def __init__(self, name, loc = loc_op()):
        self.name = name
        self.locs = [loc]
        self.type = 'bool'
        self.value = None
        self.defaults = []
        self.depends = []
        self.selects = []

    def get_name(self):
        return self.name

    def add_location(self, loc):
        self.locs.append(loc)

    def get_locations(self):
        return self.locs

    def get_location_str(self):
        lstr = ''
        for loc in self.locs:
            if lstr:
                lstr += ', '
            lstr += '[' + loc.get_op() + '] ' + loc.get_fname() + ':' + \
                    str(loc.get_line())

        return lstr

    def get_value(self):
        return self.value

    def set_value(self, val):
        self.value = val

    def add_value(self, val):
        if not self.value:
            self.value = val
        else:
            if type(self.value) != 'list':
                self.value = [self.value]
            self.value.append(val)

    def add_defaults(self, dval):
        self.defaults.append(dval)

    def get_defaults(self):
        return self.defaults

    def get_type(self):
        return self.type

    def set_type(self, typ):
        self.type = typ

    def get_depends(self):
        return self.depends

    def add_depends(self, dep):
        self.depends.append(dep)

    def get_selects(self):
        return self.selects

    def add_selects(self, sel):
        self.selects.append(sel)


class lines_set:
    def __init__(self, fname):
        self.lines = []
        kf = open(fname, 'r')
        rline = ''
        for line in kf:
            # Strip newline. Why line.rstrip() did not work here?
            nl = line.find('\n')
            if nl >= 0:
                line = line[0:nl]
            # Handle the line merge due to \ line ending
            m = re.match('(.*)\\\s*$', line)
            if m:
                rline += m.group(1)
            else:
                if rline:
                    line = rline + line;
                    rline = ''
                self.lines.append(line)
        if rline:
            self.lines.append(rline)
        kf.close()
        self.nlines = len(self.lines)
        self.pos = 0
        self.fname = fname

    def get_file_name(self):
        return self.fname

    def next(self):
        if self.eof():
            return None
        ln = self.lines[self.pos]
        self.pos += 1
        return ln

    def curr(self):
        if self.eof():
            return None
        return self.lines[self.pos]

    def length(self):
        return self.nlines

    def seek_back(self, n):
        if n > self.pos:
            raise IndexError(self.get_context() +
                             ': Cannot seek back that many lines: ' + n)
        self.pos -= n
        return self.pos

    def rewind(self):
        self.pos = 0;

    def eof(self):
        return self.pos >= self.nlines

    def get_pos(self):
        return self.pos

    def get_context(self):
        return self.fname + ':' + str(self.pos + 1)


class depgraph_node:
    def __init__(self, name):
        self.name = name
        self.dependents = dict()
        self.depending = dict()

    def get_name(self):
        return self.name

    def add_dependents(self, dep):
        self.dependents[dep.get_name()] = dep
        dep.depending[self.name] = self

    def get_dependents(self):
        return self.dependents

    def get_depending(self):
        return self.depending


def get_or_add(dobj, key, cls, *xargs):
    if key in dobj:
        return dobj[key]
    else:
        ctor = globals()[cls]
        nobj = ctor(key, *xargs)
        dobj[key] = nobj
        return nobj


def dbg_print(lev, str, newl = '\n'):
    if lev < dbg_level:
        os.sys.stderr.write(str)
        if newl:
            os.sys.stderr.write(newl)


def load_config(fname, tree):
    lineno = 0
    cf = open(fname, 'r')
    for line in cf:
        lineno += 1
        line.rstrip()
        m = re.match('CONFIG_([A-Za-z0-9_]+)\s*=\s*([^\s].*)', line)
        if m:
            config = m.group(1)
            value = m.group(2)
            dbg_print(5, 'Initializing ' + config + ' to ' + str(value))
            ke = tree_entry(config, loc_op('SET', fname, lineno))
            tree[config] = ke
            if re.match('(0x[0-9a-fA-F]+|[-+]?\d+)$', value):
                ke.set_type('int')
            elif re.match('[ynm]$', value):
                ke.set_type('tristate')
            else:
                ke.set_type('string')
            ke.set_value(value)
            continue
        m = re.match('#\s*CONFIG_([A-Za-z0-9_]+)\s+is\s+not\s+set', line)
        if m:
            config = m.group(1)
            dbg_print(5, 'Initializing ' + config + ' to \'n\'')
            ke = tree_entry(config, loc_op('SET', fname, lineno))
            ke.set_type('bool')
            ke.set_value('n')
            tree[config] = ke
            continue
    cf.close()
    return tree


def build_ktree(root, cfgfiles):
    tree = dict()
    for cfgfile in cfgfiles:
        load_config(cfgfile, tree)
    fname = os.path.join(root, 'Kconfig')
    return parse_kconfig(root, tree, fname, None)


def string_var_replace(tree, str):
    rstr = ''
    while True:
        m = re.search('\$([A-Za-z0-9_]+)', str)
        if not m:
            rstr += str
            break
        rstr += str[0:m.start()]
        key = m.group(1)
        if key in tree:
            ke = tree[key]
            if ke.get_value() is not None:
                rstr += ke.get_value()
        str = str[m.end():]
    return rstr


def want_bool(val):
    if not val or val == 'n' or val == '0':
        return False
    if val == 'y' or val == '1' or val == 'm':
        return True;
    if isinstance(val, (int, long)):
        return True if val != 0 else False
    raise SyntaxError('Bad boolean expression value: ' + str(val))


def can_be_toggled(tree, ke):
    for dep in ke.get_depends():
        v = parse_expression(tree, dep)
        if not v or not want_bool(v):
            return False
    return True


def next_token(tstr):
    m = re.match('\s*([^\s].*)', tstr)
    if m:
        tstr = m.group(1)
    if re.match('\s*$', tstr) or tstr[0] == '#':
        return [TOKEN_EOF, None, '']
    if tstr[0] == '=':
        return [TOKEN_EQ, '=', tstr[1:]]
    m = re.match('!=\s*([^\s].*)', tstr)
    if m:
        return [TOKEN_NEQ, '!=', m.group(1)]
    if tstr[0] == '!':
        return [TOKEN_LNOT, '!', tstr[1:]]
    if tstr[0] == '(':
        return [TOKEN_OPAR, '(', tstr[1:]]
    if tstr[0] == ')':
        return [TOKEN_CPAR, ')', tstr[1:]]
    m = re.match('&&\s*([^\s].*)', tstr)
    if m:
        return [TOKEN_LAND, '&&', m.group(1)]
    m = re.match('\|\|\s*([^\s].*)', tstr)
    if m:
        return [TOKEN_LOR, '||', m.group(1)]
    m = re.match('if\s+([^\s].*)', tstr)
    if m:
        return [TOKEN_IF, 'if', m.group(1)]
    m = re.match('(\d+)([^a-zA-Z_0-9].*)?$', tstr)
    if m:
        return [TOKEN_LITERAL, m.group(1), (m.group(2) or '')]
    m = re.match('(0x[0-9a-fA-F]+)([^a-zA-Z_0-9].*)?$', tstr)
    if m:
        return [TOKEN_LITERAL, m.group(1), (m.group(2) or '')]
    m = re.match('([yn])([^a-zA-Z_0-9].*)?$', tstr)
    if m:
        return [TOKEN_LITERAL, m.group(1), (m.group(2) or '')]
    m = re.match('"([^"]*)"', tstr)
    if m:
        return [TOKEN_LITERAL, m.group(1), tstr[m.end():]]
    m = re.match('\'([^\']*)\'', tstr)
    if m:
        return [TOKEN_LITERAL, m.group(1), tstr[m.end():]]
    m = re.match('[a-zA-Z0-9_]+', tstr)
    if m:
        name = m.group(0)
        # Kconfig uses some sort names, like 'm' for MODULES for example.
        # This normalizes back to CONFIG_ names.
        if name in remap_dict:
            name = remap_dict[name]
        return [TOKEN_NAME, name, tstr[m.end():]]
    raise SyntaxError('Bad expression: ' + tstr)


def parse_leaf_expression(tree, expr, acl):
    n = next_token(expr)
    if n[0] == TOKEN_OPAR:
        s = parse_full_expression(tree, n[2], acl)
        n = next_token(s[2])
        if n[0] != TOKEN_CPAR:
            raise SyntaxError('Mismatch parenthesis: ' + expr)
        return [s[0], s[1], n[2]]
    if n[0] == TOKEN_LNOT:
        s = parse_leaf_expression(tree, n[2], acl)
        if s[0] != TOKEN_VALUE:
            raise SyntaxError('Logical not target not a value: ' + expr)
        bv = want_bool(s[1])
        if bv:
            return [TOKEN_VALUE, 'n', s[2]]
        else:
            return [TOKEN_VALUE, 'y', s[2]]
    if n[0] == TOKEN_LITERAL:
        return [TOKEN_VALUE, n[1], n[2]]
    if n[0] == TOKEN_NAME:
        key = n[1]
        value = 'n'
        if key in tree:
            ke = tree[key]
            value = ke.get_value() or 'n'
        if acl is not None:
            acl[key] = value
        return [TOKEN_VALUE, value, n[2]]
    raise SyntaxError('Bad expression: ' + expr)


def dump_acl_values(tree, acl, lev = 5, prestr = '\t'):
    for key in acl:
        if not key in tree:
            dbg_print(lev, prestr + 'Missing ' + key + ' config, using \'n\'')
        else:
            ke = tree[key]
            loc = ke.get_location_str()
            dbg_print(lev, prestr + 'Accessing ' + key + ' (' + loc + ') = ' +
                      str(ke.get_value()))


def parse_cmp_expression(tree, expr, acl):
    t = parse_leaf_expression(tree, expr, acl)
    n = next_token(t[2])
    if n[0] == TOKEN_EQ:
        s = parse_leaf_expression(tree, n[2], acl)
        if t[1] == s[1]:
            return [TOKEN_VALUE, 'y', s[2]]
        else:
            return [TOKEN_VALUE, 'n', s[2]]
    if n[0] == TOKEN_NEQ:
        s = parse_leaf_expression(tree, n[2], acl)
        if t[1] != s[1]:
            return [TOKEN_VALUE, 'y', s[2]]
        else:
            return [TOKEN_VALUE, 'n', s[2]]
    return t


def parse_and_expression(tree, expr, acl):
    t = parse_cmp_expression(tree, expr, acl)
    while True:
        n = next_token(t[2])
        if n[0] != TOKEN_LAND:
            break
        s = parse_cmp_expression(tree, n[2], acl)
        if want_bool(t[1]) and want_bool(s[1]):
            t[1] = 'y'
        else:
            t[1] = 'n'
        t[2] = s[2]
    return t


def parse_or_expression(tree, expr, acl):
    t = parse_and_expression(tree, expr, acl)
    while True:
        n = next_token(t[2])
        if n[0] != TOKEN_LOR:
            break
        s = parse_and_expression(tree, n[2], acl)
        if want_bool(t[1]) or want_bool(s[1]):
            t[1] = 'y'
        else:
            t[1] = 'n'
        t[2] = s[2]
    return t


def parse_if_expression(tree, expr, acl):
    t = parse_or_expression(tree, expr, acl)
    n = next_token(t[2])
    if n[0] == TOKEN_IF:
        s = parse_or_expression(tree, n[2], acl)
        if want_bool(s[1]):
            return [t[0], t[1], s[2]]
        else:
            return [TOKEN_VALUE, None, s[2]]
    return t


def parse_full_expression(tree, expr, acl):
    return parse_if_expression(tree, expr, acl)


def parse_expression(tree, expr, acl = dict()):
    dbg_print(5, 'Expr: ' + expr)
    t = parse_full_expression(tree, expr, acl)
    dump_acl_values(tree, acl)
    dbg_print(9, '\tValue = ' + str(t[1]))
    return t[1]


def parse_config_option(lset, root, tree, ke):
    while not lset.eof():
        line = lset.next()
        m = re.match('[^\s]+', line)
        if m:
            lset.seek_back(1)
            return
        m = re.match('\s+string(\s.*)?$', line)
        if m:
            ke.set_type('string')
            continue
        m = re.match('\s+bool(\s.*)?$', line)
        if m:
            ke.set_type('bool')
            if m.group(1):
                m = re.match('\s+"[^"]*"\s+if\s+(.*)', m.group(1))
                if m:
                    ke.add_depends(m.group(1))
            continue
        m = re.match('\s+tristate(\s.*)?$', line)
        if m:
            ke.set_type('tristate')
            continue
        m = re.match('\s+def_bool\s+([^\s].*)$', line)
        if m:
            ke.set_type('bool')
            ke.add_defaults(m.group(1))
            continue
        m = re.match('\s+int(\s.*)?$', line)
        if m:
            ke.set_type('int')
            continue
        m = re.match('\s+option\s+([^\n]+)', line)
        if m:
            ostr = m.group(1)
            m = re.match('env="([^"]+)"', ostr)
            if m:
                if m.group(1) in os.environ:
                    ke.set_value(os.environ[m.group(1)])
                else:
                    ke.set_value('')
            continue
        m = re.match('\s+default\s+([^\n]+)', line)
        if m:
            ke.add_defaults(m.group(1))
            continue
        m = re.match('\s+depends\s+on\s+([^\n]+)', line)
        if m:
            ke.add_depends(m.group(1))
            continue
        m = re.match('\s+select\s+([^\n]+)', line)
        if m:
            ke.add_selects(m.group(1))
            continue
        m = re.match('\s+help(\s.*)?$', line)
        if m:
            while not lset.eof():
                line = lset.next()
                m = re.match('[^\s]+', line)
                if m:
                    lset.seek_back(1)
                    return


def parse_menu(lset, root, tree, dchain):
    while not lset.eof():
        line = lset.next()
        m = re.match('[^\s]+', line)
        if m:
            lset.seek_back(1)
            return
        m = re.match('\s+depends\s+on\s+([^\n]+)', line)
        if m:
            dchain.add_depends(m.group(1))
            continue


def parse_kconfig(root, tree, fname, dchain):
    dbg_print(3, 'Parsing ' + fname)
    lset = lines_set(fname)
    if_nest = 0
    menu_nest = 0
    choice_nest = 0
    while not lset.eof():
        line = lset.next()
        m = re.match('(config|menuconfig)\s+([A-Za-z0-9_]+)', line)
        if m:
            config = m.group(2)
            if config in tree:
                ke = tree[config]
                ke.add_location(loc_op('DEF', fname, lset.get_pos()))
            else:
                ke = tree_entry(config, loc_op('DEF', fname, lset.get_pos()))
                tree[config] = ke
                c = dchain
                while c:
                    ke.get_depends().extend(c.get_depends())
                    c = c.get_next()
            parse_config_option(lset, root, tree, ke)
            continue
        m = re.match('if\s+([^\s].*[^\s])\s*$', line)
        if m:
            dchain = depend_chain(dchain)
            dchain.add_depends(m.group(1))
            if_nest += 1
            continue
        m = re.match('endif(\s.*)?$', line)
        if m:
            if_nest -= 1
            if if_nest < 0:
                dbg_print(0, lset.get_context())
                dbg_print(0, 'Nested IF not matching: ' + str(if_nest))
                exit(2)
            dchain = dchain.get_next()
            continue
        m = re.match('menu(\s.*)?$', line)
        if m:
            dchain = depend_chain(dchain)
            parse_menu(lset, root, tree, dchain)
            menu_nest += 1
            continue
        m = re.match('endmenu(\s.*)?$', line)
        if m:
            menu_nest -= 1
            if menu_nest < 0:
                dbg_print(0, lset.get_context())
                dbg_print(0, 'Nested MENU not matching: ' + str(menu_nest))
                exit(2)
            dchain = dchain.get_next()
            continue
        m = re.match('choice(\s.*)?$', line)
        if m:
            dchain = depend_chain(dchain)
            parse_menu(lset, root, tree, dchain)
            choice_nest += 1
            continue
        m = re.match('endchoice(\s.*)?$', line)
        if m:
            choice_nest -= 1
            if choice_nest < 0:
                dbg_print(0, lset.get_context())
                dbg_print(0, 'Nested CHOICE not matching: ' + str(choice_nest))
                exit(2)
            dchain = dchain.get_next()
            continue
        m = re.match('source\s+"([^"]+)"', line)
        if not m:
            m = re.match('source\s+([^\s]+)', line)
        if m:
            spath = string_var_replace(tree, m.group(1))
            tree = parse_kconfig(root, tree, os.path.join(root, spath),
                                 dchain)
            continue
    if if_nest > 0:
        dbg_print(0, lset.get_context())
        dbg_print(0, 'Nested IF not matching: ' + str(if_nest))
        exit(2)
    if menu_nest > 0:
        dbg_print(0, lset.get_context())
        dbg_print(0, 'Nested MENU not matching: ' + str(menu_nest))
        exit(2)
    if choice_nest > 0:
        dbg_print(0, lset.get_context())
        dbg_print(0, 'Nested CHOICE not matching: ' + str(choice_nest))
        exit(2)
    return tree


def dump_ktree(tree, cf):
    for key in tree.keys():
        ke = tree[key]
        value = ke.get_value()
        if not value or value == 'n':
            cf.write('# CONFIG_' + key + ' is not set\n')
        else:
            cf.write('CONFIG_' + key + '=' + str(value) + '\n')


def apply_config_defaults(tree, ke):
    applied = 0
    if not ke.get_value():
        for dval in ke.get_defaults():
            v = parse_expression(tree, dval)
            if not v:
                continue
            if (v == 'y' or v == 'm') and not can_be_toggled(tree, ke):
                continue
            dbg_print(5, 'Defaulting ' + ke.get_name() + ' to ' + str(v))
            ke.add_value(v)
            applied += 1
    return applied


def apply_config_selects(tree, ke):
    applied = 0
    value = ke.get_value()
    if value == 'y' or value == 'm':
        for sel in ke.get_selects():
            # A select might or might not, come with an IF conditional
            m = re.match('([A-Za-z0-9_]+)$', sel)
            if m:
                sname = m.group(1)
            else:
                m = re.match('([A-Za-z0-9_]+)\s+if\s+(.*)', sel)
                if m:
                    sname = m.group(1)
                    v = parse_expression(tree, m.group(2))
                    if not (v and want_bool(v)):
                        continue
                else:
                    continue
            ske = get_or_add(tree, sname, 'tree_entry')
            if can_be_toggled(tree, ske) and ske.get_value() != 'y':
                dbg_print(5, 'Activating ' + ske.get_name())
                ske.set_value('y')
                applied += 1
    return applied


def populate_depend_graph(dgraph, acl, dgn, okey = None):
    for akey in acl.keys():
        if akey != okey:
            xdgn = get_or_add(dgraph, akey, 'depgraph_node')
            xdgn.add_dependents(dgn)


def build_depend_graph(tree):
    dgraph = dict()
    for key in tree.keys():
        ke = tree[key]
        dgn = get_or_add(dgraph, key, 'depgraph_node')
        # Add dependencies from explicit 'depends on'
        for dep in ke.get_depends():
            acl = dict()
            parse_expression(tree, dep, acl)
            populate_depend_graph(dgraph, acl, dgn)
        # Add dependencies due to default value expression evaluation
        for dval in ke.get_defaults():
            acl = dict()
            parse_expression(tree, dval, acl)
            populate_depend_graph(dgraph, acl, dgn)
        # Add dependencies due to 'selects X if EXPR' expression evaluation
        for sel in ke.get_selects():
            m = re.match('([A-Za-z0-9_]+)(\s+if\s+(.*))?', sel)
            if m:
                tdgn = get_or_add(dgraph, m.group(1), 'depgraph_node')
                dgn.add_dependents(tdgn)
                if m.group(3):
                    acl = dict()
                    parse_expression(tree, m.group(3), acl)
                    populate_depend_graph(dgraph, acl, dgn, key)
    return dgraph


def resolve_depending(tree, dgraph, resolved, tlk, dgn):
    name = dgn.get_name()
    if name in resolved:
        if resolved[name] == 1:
            # We add as dependency for config FOO, even the expressions in the
            # 'select if EXPR' cases. This allows for the EXPR dependencies to
            # be resolved before EXPR is evaluated. There are cases though where
            # for config FOO, we have 'select X if FOO = m', which is OK because
            # below we first apply the defaults, and then evaluate the selects.
            # Other cases exists, which are not that trivial, but still are fine
            # from a dependency POV.
            # IOW a circular dependency print is not necessarily a bad
            # condition.
            dbg_print(5, 'Circular dependency loop detected at ' + name +
                      ' coming from ' + str(tlk))
        return
    else:
        resolved[name] = 1
    dbg_print(6, 'Resolving ' + name)
    tlk.append(name)
    deps = dgn.get_depending()
    for key in deps.keys():
        resolve_depending(tree, dgraph, resolved, tlk, deps[key])
    tlk.pop()
    resolved[name] = 2
    dbg_print(6, 'Resolved ' + name)
    if name in tree:
        ke = tree[name]
        apply_config_defaults(tree, ke)
        apply_config_selects(tree, ke)


def do_values_propagate(tree):
    dgraph = build_depend_graph(tree)
    resolved = dict()
    for key in dgraph.keys():
        resolve_depending(tree, dgraph, resolved, [], dgraph[key])


def do_drop_unsatisfied(tree):
    dropped = 0;
    for key in tree.keys():
        ke = tree[key]
        value = ke.get_value()
        if not (value == 'y' or value == 'm'):
            continue
        for dep in ke.get_depends():
            acl = dict()
            v = parse_expression(tree, dep, acl)
            if v is None or v == 'n':
                dbg_print(0, 'Option ' + ke.get_name() +
                          ' is set but depends on ' + dep)
                for akey in acl.keys():
                    dbg_print(0, '\t' + akey + ' = ' + str(acl[akey]))
                dbg_print(0, 'Turning off ' + ke.get_name())
                ke.set_value(None)
                dropped += 1
    return dropped


def verify_ktree(tree):
    do_values_propagate(tree)
    while True:
        done = do_drop_unsatisfied(tree)
        if done == 0:
            break;


def check_config(tree, configs):
    for cfg in configs:
        if not cfg in tree:
            dbg_print(0, 'Missing config in parsed tree: ' + cfg)
            continue
        ke = tree[cfg]
        dbg_print(0, '\nConfig     : ' + cfg)
        dbg_print(0, 'Defined In : ' + ke.get_location_str())
        dbg_print(0, 'Value      : ' + str(ke.get_value()))
        for dep in ke.get_depends():
            dbg_print(0, 'Depends On : ' + dep + ' ... ', newl = None)
            acl = dict()
            v = parse_expression(tree, dep, acl)
            if v is None or v == 'n':
                dbg_print(0, 'FAILED')
            else:
                dbg_print(0, 'OK')
            dump_acl_values(tree, acl, 0)
        for dval in ke.get_defaults():
            dbg_print(0, 'Defaults To: ' + dval + ' ... ', newl = None)
            acl = dict()
            v = parse_expression(tree, dval, acl)
            if not v:
                dbg_print(0, 'NACK')
            else:
                dbg_print(0, 'OK')
            dump_acl_values(tree, acl, 0)


def main():
    import argparse

    global dbg_level

    parser = argparse.ArgumentParser()
    parser.add_argument('-i', '--incfg', type=str, action='append', default=[],
                        help='Adds one configuration file')
    parser.add_argument('-r', '--root', type=str, default='.',
                        help='Sets the source root directory')
    parser.add_argument('-c', '--cmd', type=str, default='verify',
                        help='Run the verify command')
    parser.add_argument('-C', '--configs', type=str, action='append', default=[],
                        help='Sets the config name(s) to check')
    parser.add_argument('-o', '--outcfg', type=str,
                        help='Sets the output configuration file ("-" means STDOUT)')
    parser.add_argument('-L', '--log_level', type=int, default=1,
                        help='Sets the logging level')
    args = parser.parse_args()

    dbg_level = args.log_level
    gfiles = args.incfg
    gfiles.extend(sorted(glob.glob(os.path.join(args.root, 'Gconfig.*'))))
    ktree = build_ktree(args.root, gfiles)
    if args.cmd == 'verify':
        verify_ktree(ktree)
    elif args.cmd == 'checkcfg':
        if not args.configs:
            dbg_print(0, 'Missing config name to check (-C)')
        check_config(ktree, args.configs)
    else:
        dbg_print(0, 'Unknown command: ' + args.cmd)
        exit(2)

    if args.outcfg is not None:
        if args.outcfg != '-':
            cf = open(args.outcfg, 'w')
            dump_ktree(ktree, cf)
            cf.close()
        else:
            dump_ktree(ktree, os.sys.stdout)


if __name__ == '__main__':
    main()
