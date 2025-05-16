package main

import (
	"fmt"
	"os"
	"slices"
	"unicode"
	"strings"
)

type ExprType int

const (
	Assign = iota
	Call
	Func
	Print
	Var
	Plhr
)

type Expr struct {
	t        ExprType
	char     rune
	children []Expr
}

func lexer(input string) []Expr {
	toks := make([]Expr, len(input))
	for _, char := range input {
		if char == '!' {
			toks = append(toks, Expr{t: Plhr, char: '!'})
		} else if char == '(' {
			toks = append(toks, Expr{t: Plhr, char: '('})
		} else if char == ')' {
			toks = append(toks, Expr{t: Plhr, char: ')'})
		} else if char == '=' {
			toks = append(toks, Expr{t: Plhr, char: '='})
		} else if char == '.' {
			toks = append(toks, Expr{t: Plhr, char: '.'})
		} else if unicode.IsSpace(char) == true {
		} else {
			toks = append(toks, Expr{t: Var, char: char})
		}
	}

	return toks
}

func pe(es []Expr) bool {
	for _, expr := range es {
		if expr.t == Var || expr.t == Plhr {
			return false
		}
	}
	return true
}

func ive(x Expr) bool {
	return x.t == Var || x.t == Call || x.t == Func
}

func ra(es []Expr) ([]Expr, bool) {
	for i := 0; i < len(es)-2; i++ {
		if es[i].t == Var && es[i+1].t == Plhr && es[i+1].char == '=' && ive(es[i+2]) {
			if i == 0 {
				return append([]Expr{{t: Assign, children: []Expr{es[i], es[i+2]}}}, es[3:]...), true
			} else if i == len(es)-3 {
				return append(es[:len(es)-3], []Expr{{t: Assign, children: []Expr{es[i], es[i+2]}}}...), true
			} else {
				return append(append(es[:i], []Expr{{t: Assign, children: []Expr{es[i], es[i+2]}}}...), es[i+3:]...), true
			}
		}
	}
	return es, false
}

func rc(es []Expr) ([]Expr, bool) {
	for i := 0; i < len(es)-3; i++ {
		if ive(es[i+1]) && ive(es[i+2]) && es[i].t == Plhr && es[i].char == '(' && es[i+3].t == Plhr && es[i+3].char == ')' {
			if i == 0 {
				return append([]Expr{{t: Call, children: []Expr{es[i+1], es[i+2]}}}, es[4:]...), true
			} else if i == len(es)-4 {
				return append(es[:len(es)-4], []Expr{{t: Call, children: []Expr{es[i+1], es[i+2]}}}...), true
			} else {
				return append(append(es[:i], []Expr{{t: Call, children: []Expr{es[i+1], es[i+2]}}}...), es[i+4:]...), true
			}
		}
	}
	return es, false
}

func rf(es []Expr) ([]Expr, bool) {
	for i := 0; i < len(es)-3; i++ {
		if ive(es[i+1]) && ive(es[i+3]) && es[i].t == Plhr && es[i].char == '!' && es[i+2].t == Plhr && es[i+2].char == '.' {
			if i == 0 {
				return append([]Expr{{t: Func, children: []Expr{es[i+1], es[i+3]}}}, es[4:]...), true
			} else if i == len(es)-4 {
				return append(es[:len(es)-4], []Expr{{t: Func, children: []Expr{es[i+1], es[i+3]}}}...), true
			} else {
				return append(append(es[:i], []Expr{{t: Func, children: []Expr{es[i+1], es[i+3]}}}...), es[i+4:]...), true
			}
		}
	}
	return es, false
}

func rp(es []Expr) ([]Expr, bool) {
	for i := 0; i < len(es)-2; i++ {
		if ive(es[i+2]) && es[i].t == Plhr && es[i].char == '!' && es[i+1].t == Plhr && es[i+1].char == '=' {
			if i == 0 {
				return append([]Expr{{t: Print, children: []Expr{es[i+2]}}}, es[3:]...), true
			} else if i == len(es)-3 {
				return append(es[:len(es)-3], []Expr{{t: Print, children: []Expr{es[i+2]}}}...), true
			} else {
				return append(append(es[:i], []Expr{{t: Print, children: []Expr{es[i+2]}}}...), es[i+3:]...), true
			}
		}
	}
	return es, false
}

func parser(tokens []Expr) []Expr {
	end := false
	fa, fc, ff, fp := false, false, false, false
	for !(end) {
		tokens, fa = ra(tokens)
		tokens, fc = rc(tokens)
		tokens, ff = rf(tokens)
		tokens, fp = rp(tokens)
		if !(fa || fc || ff || fp || end) {
			panic("syntax error: extra identifier or other symbol")
		}
		end = pe(tokens)
	}

	return tokens
}

func ce(es []Expr) []Expr {
	out := make([]Expr, 0)
	for _, e := range es {
		if !(e.t == 0 && e.char == 0 && len(e.children) == 0) {
			out = append(out, e)
		}
	}
	return out
}

func mast(code string) []Expr {
	return ce(parser(lexer(code)))
}

type Function struct {
	argname rune
	expr    Expr
	ds      Scope
}

type Scope struct {
	vars map[rune]Function
}

type LScope struct {
	sltg []*Scope
}

func contains(slice []rune, item rune) bool {
	for _, v := range slice {
		if v == item {
			return true
		}
	}
	return false
}

func remove(slice []rune, value rune) []rune {
	for i, v := range slice {
		if v == value {
			return append(slice[:i], slice[i+1:]...)
		}
	}
	return slice // not found; return original
}

func getvars(e Expr) []rune {
	if e.t == Func {
		d := getvars(e.children[1])
		rmd := remove(d, e.children[0].char)
		return rmd
	} else if e.t == Call {
		apd := getvars(e.children[0])
		for _, i := range getvars(e.children[1]) {
			if !(contains(apd, i)) {
				apd = append(apd, i)
			}
		}
		return apd
	} else if e.t == Var {
		return []rune{e.char}
	}
	panic("interpreter internal error 3")
}

func evalexpr(e Expr, scope *LScope, ce Expr) Function {
	if e.t == Func {
		arg := e.children[0].char
		ex := e.children[1]
		nv := getvars(e)
		sc := Scope{vars: make(map[rune]Function)}
		for _, v := range nv {
			sc.vars[v] = gvfe(v, scope, ce)
		}
		return Function{argname: arg, expr: ex, ds: sc}
	} else if e.t == Call {
		f := evalexpr(e.children[0], scope, ce)
		an := f.argname
		arg := evalexpr(e.children[1], scope, ce)
		ns := LScope{sltg: make([]*Scope, 0)}
		for _, s := range (*scope).sltg {
			ns.sltg = append(ns.sltg, s)
		}
		tm := make(map[rune]Function)
		tm[an] = arg
		ns.sltg = append(ns.sltg, &Scope{vars: tm}, &(f.ds))
		return evalexpr(f.expr, &ns, ce)
	} else if e.t == Var {
		return gvfe(e.char, scope, ce)
	}
	panic("interpreter internal error 2")
}

func gvfe(n rune, scope *LScope, ce Expr) Function {
	slices.Reverse(scope.sltg)
	for _, scl := range scope.sltg {
		val, ok := scl.vars[n]
		if ok {
			slices.Reverse(scope.sltg)
			return val
		}
	}
	var cure Expr
	cure = ce
	if cure.t == Assign {
		cure = cure.children[1]
	}
	if cure.t == Print {
		cure = cure.children[0]
	}
	
	fmt.Println("Error in :", prex(false, cure, &LScope{sltg: make([]*Scope, 0)}, ce))
	panic("undefined variable " + string(n))
}

func printify(f Function, scope *LScope, cure Expr, un ...rune) string {
	un = append(un, f.argname)
	scope.sltg = append(scope.sltg, &f.ds)
	return "! " + string(f.argname) + ". " + prex(true, f.expr, scope, cure, un...)
}

func prex(ev bool, e Expr, scope *LScope, cure Expr, un ...rune) string {
	if e.t == Func {
		un = append(un, e.children[0].char)
		return "! " + string(e.children[0].char) + ". " + prex(ev, e.children[1], scope, cure, un...)
	} else if e.t == Call {
		return "( " + prex(ev, e.children[0], scope, cure, un...) + " " + prex(ev, e.children[1], scope, cure, un...) + " )"
	} else if e.t == Var {
		if slices.Contains(un, e.char) || !ev {
			return string(e.char)
		}
		return printify(evalexpr(e, scope, cure), scope, cure, un...)
	}
	fmt.Println(e.t)
	panic("interpreter internal error 1")
}

func runapexpr(e Expr, scope *LScope, out []string) []string {
	if e.t == Assign {
		(*scope).sltg[len((*scope).sltg)-1].vars[e.children[0].char] = evalexpr(e.children[1], scope, e.children[1])
		return out
	} else if e.t == Print {
		o := printify(evalexpr(e.children[0], scope, e.children[0]), scope, e)
		fmt.Println(o)
		return append(out, o)
	}
	fmt.Println(prex(false, e, scope, e.children[1]))
	panic("statement has no effect on program")
}

func execast(es []Expr) []string {
	output := []string{}
	scope := LScope{sltg: []*Scope{{vars: make(map[rune]Function)}}}
	for _, e := range es {
		output = runapexpr(e, &scope, output)
	}
	return output
}

func runfile(extracode []string, fn string) ([]string, error) {
	fmt.Println("---------- running file", fn)
	code, err := os.ReadFile(fn)
	if err != nil {
		fmt.Println("Error while reading", os.Args[1], ":", err)
		return []string{}, nil
	}
	incode := string(code)
	for _, i := range extracode {
		incode = i + incode
	}
	ast := mast(incode)
	output := execast(ast)
	return output, nil
}

func main() {
	if len(os.Args) < 2 {
		fmt.Println("try again but with code file name as cli arg")
		return
	}
	fn := os.Args[1]
	output, err := runfile([]string{}, fn)
	if err != nil {
		panic("Error in file " + fn + fmt.Sprint(err))
	}
	if len(os.Args) > 2 && len(os.Args)%3 == 2 {
		for i := 1; i < (len(os.Args)+1)/3; i++ {
			if os.Args[i*3-1] != "-oi" || strings.ContainsRune(os.Args[i*3], '!') || strings.ContainsRune(os.Args[i*3], '.') || strings.ContainsRune(os.Args[i*3], '(') || strings.ContainsRune(os.Args[i*3], ')') || strings.ContainsRune(os.Args[i*3], '=') {
				panic("invalid cli syntax")
			}
			if len(output) != len(os.Args[i*3]) {
				panic("wrong number of identifier names in file chain link")
			}
			fn := os.Args[i*3+1]
			inputv := []string{}
			for j, val := range output {
				inputv = append(inputv, string(os.Args[i*3][j]) + "=" + val)
			}
			output, err = runfile(inputv, fn)
			if err != nil {
				panic("Error in file " + fn + fmt.Sprint(err))
			}
		}
		return
	}
	if len(os.Args) > 2{
		panic("invalid cli syntax")
	}
}
