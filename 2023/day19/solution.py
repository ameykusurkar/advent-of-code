raw_program, raw_args = open(0).read().split("\n\n")
workflows = {}
for p in raw_program.splitlines():
    name, rest = p.split('{')
    raw_rules = rest[:-1].split(",")

    rules = []
    for r in raw_rules:
        match r.split(":"):
            case [cond, label]:
                s = cond.split("<")
                if len(s) > 1:
                    rules.append(((s[0], "<", int(s[1])), label))
                else:
                    s = cond.split(">")
                    rules.append(((s[0], ">", int(s[1])), label))
            case [label]:
                rules.append((None, label))
    workflows[name] = rules

args_list = []
for arg in raw_args.splitlines():
    a = {}
    for pair in arg[1:-1].split(','):
        k, v = pair.split('=')
        a[k] = int(v)
    args_list.append(a)

def work(args, w):
    for (cond, label) in w:
        if not cond:
            return label
        a, sign, val = cond
        if sign == '<' and args[a] < val:
                return label
        elif sign == '>' and args[a] > val:
                return label
    assert(False)

total = 0
for args in args_list:
    label = 'in'
    while True:
        match work(args, workflows[label]):
            case 'A':
                total += sum(args[k] for k in args)
                break
            case 'R':
                break
            case l:
                label = l
print(total)
