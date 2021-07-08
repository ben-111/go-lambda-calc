package lambda

import (
	"fmt"
	"strconv"
)

// SubstitutionError is
type SubstitutionError struct {
	Param   Variable
	Expr    LambdaExpression
	Convert *Abstraction
	Avoid   map[Variable]struct{}
}

// BetaNormalError is
type BetaNormalError struct {
	Expr LambdaExpression
}

// RedexError is
type RedexError struct {
	Expr LambdaExpression
}

func (e SubstitutionError) Error() string {
	return fmt.Sprintf("SubstitutionError: Cannot substitute variable in expression because %v occurs free in %v", e.Param.PrettyString(), e.Expr.PrettyString())
}

func (e BetaNormalError) Error() string {
	return fmt.Sprintf("BetaNormalError: Cannot reduce any further because the expression %v is of Beta-Normal form", e.Expr.PrettyString())
}

func (e RedexError) Error() string {
	return fmt.Sprintf("RedexError: expression %v is not a Beta-Redex", e.Expr)
}

// GetFreeVariablesNames gets the set of free variables in the lambda expression
func GetFreeVariablesNames(expr LambdaExpression) map[Variable]struct{} {
	freevars := map[Variable]struct{}{}
	switch e := expr.(type) {
	// Free variables of x are just x
	case *Variable:
		freevars[*e] = struct{}{}

	// The free variables of λx.M are the set of free variables of M, with x removed.
	case *Abstraction:
		absfreevars := GetFreeVariablesNames(e.Output)
		for k := range absfreevars {
			freevars[k] = struct{}{}
		}
		delete(freevars, e.Param)
	// The free variables of (M N) is the union of the free variables of M and N.
	case *Application:
		funcfreevars := GetFreeVariablesNames(e.Function)
		for k, v := range funcfreevars {
			freevars[k] = v
		}
		argsfreevars := GetFreeVariablesNames(e.Argument)
		for k, v := range argsfreevars {
			freevars[k] = v
		}
	}

	return freevars
}

// GetFreeVariables gets
func (v *Variable) GetFreeVariables() (free []*Variable) {
	free = append(free, v)
	return
}

// GetFreeVariables gets
func (a *Abstraction) GetFreeVariables() (free []*Variable) {
	free = a.Output.GetFreeVariables()
	free = removeVariablePointer(free, &a.Param)
	return
}

// GetFreeVariables gets
func (a *Application) GetFreeVariables() (free []*Variable) {
	free = append(free, a.Function.GetFreeVariables()...)
	free = append(free, a.Argument.GetFreeVariables()...)
	return
}

func removeVariablePointer(vars []*Variable, rem *Variable) (removed []*Variable) {
	for i := 0; i < len(vars); i++ {
		if *(vars[i]) != *rem {
			removed = append(removed, vars[i])
		}
	}
	return
}

// RenameBoundVariables renames the bound variables of the abstraction
// All the free variables in the abstraction's output that have the same name
// as the abstraction's parameter are bound to the abstraction
func (a *Abstraction) RenameBoundVariables(rename Variable) (err error) {
	funcfreevars := a.Output.GetFreeVariables()
	for i := range funcfreevars {
		if *funcfreevars[i] == a.Param {
			*funcfreevars[i] = rename
		}
	}
	a.Param = rename
	return
}

func removeVariable(vars []Variable, rem Variable) []Variable {
	for i, vari := range vars {
		if vari == rem {
			vars[len(vars)-1], vars[i] = vars[i], vars[len(vars)-1]
			vars = vars[:len(vars)-1]
			break
		}
	}

	return vars
}

func unionVariables(a []Variable, b []Variable) []Variable {
	unionmap := map[Variable]int{}
	concat := append(a, b...)
	for _, vari := range concat {
		unionmap[vari] = 0
	}

	keys := make([]Variable, len(unionmap))
	i := 0
	for k := range unionmap {
		keys[i] = k
		i++
	}
	return keys
}

// SplitVariableName splits the variable name into the prefix letter and suffix number
func SplitVariableName(v Variable) (string, int) {
	prefix := ""
	suffix := -1
	if len(v.Name) > 0 {
		prefix = string(v.Name[0])
	}
	if len(v.Name) > 1 {
		suffix, _ = strconv.Atoi(v.Name[1:])
	}
	return prefix, suffix
}

// AlphaConvert alpha-converts the abstraction while avoiding "Avoid" names as well
func (a *Abstraction) AlphaConvert(Avoid map[Variable]struct{}) {
	// Avoid[a.Param] = struct{}{}
	freevarnames := GetFreeVariablesNames(a.Output)
	for k := range freevarnames {
		Avoid[k] = struct{}{}
	}

	// Renaming by adding a/to a number following the variable letter
	nameprefix, namesuffix := SplitVariableName(a.Param)
	namesuffix++

	suffixstr := ""
	for vari := range Avoid {
		if namesuffix >= 0 {
			suffixstr = strconv.Itoa(namesuffix)
		}
		name := fmt.Sprintf("%v%v", nameprefix, suffixstr)
		if vari.Name == name {
			namesuffix++
		}
	}
	if namesuffix >= 0 {
		suffixstr = strconv.Itoa(namesuffix)
	}

	name := fmt.Sprintf("%v%v", nameprefix, suffixstr)
	a.RenameBoundVariables(Variable{name})
}

// Substitute does that
func Substitute(Replace Variable, With LambdaExpression, In *LambdaExpression) (err error) {
	switch e := (*In).(type) {
	case *Variable:
		if *e == Replace {
			*In = With
		} // else it stays the same
	case *Abstraction:
		if (*e).Param != Replace {
			freevarnames := GetFreeVariablesNames(With)
			_, ok := freevarnames[(*e).Param]
			if !ok {
				Substitute(Replace, With, &(*e).Output) // Check if works
			} else {
				err = SubstitutionError{(*e).Param, With, e, freevarnames}
			}
		} // else it stays the same
	case *Application:
		Substitute(Replace, With, &(*e).Function)
		Substitute(Replace, With, &(*e).Argument)
	}
	return
}

// IsBetaNormalForm checks if the expression is of Beta-Normal form.
func (v *Variable) IsBetaNormalForm() bool {
	return true
}

// IsBetaNormalForm checks if the expression is of Beta-Normal form.
func (a *Abstraction) IsBetaNormalForm() bool {
	return (a.Output).IsBetaNormalForm()
}

// IsBetaNormalForm checks if the expression is of Beta-Normal form.
func (a *Application) IsBetaNormalForm() bool {
	_, ok := a.Function.(*Abstraction)
	if ok {
		return false
	}
	return a.Function.IsBetaNormalForm() && a.Argument.IsBetaNormalForm()
}

// BetaReduce reduces beta-redex
func BetaReduce(expr *LambdaExpression) (err error) {
	switch e := (*expr).(type) {
	case *Variable:
		return RedexError{e}
	case *Abstraction:
		return RedexError{e}
	case *Application:
		abs, ok := e.Function.(*Abstraction)
		if ok {
			body := abs.Output
			err = Substitute(abs.Param, e.Argument, &body)
			if err == nil {
				*expr = body
			}
		} else {
			err = RedexError{e}
		}
	}
	return
}

// ApplicativeBetaReduce - Take normal order beta-reduction step
// Leftmost innermost
func ApplicativeBetaReduce(expr *LambdaExpression) (err error) {
	switch e := (*expr).(type) {
	case *Variable:
		err = BetaNormalError{e}
	case *Abstraction:
		if !e.IsBetaNormalForm() {
			return ApplicativeBetaReduce(&e.Output)
		}
		err = BetaNormalError{e}
	case *Application:
		if !e.IsBetaNormalForm() {
			if !e.Function.IsBetaNormalForm() {
				return ApplicativeBetaReduce(&e.Function)
			}
			if !e.Argument.IsBetaNormalForm() {
				return ApplicativeBetaReduce(&e.Argument)
			}
			return BetaReduce(expr)
		}
		err = BetaNormalError{e}
	}
	return
}

// NormalBetaReduce - Take normal order beta-reduction step
// Leftmost outermost
func NormalBetaReduce(expr *LambdaExpression) (err error) {
	switch e := (*expr).(type) {
	case *Variable:
		err = BetaNormalError{e}
	case *Abstraction:
		if !e.IsBetaNormalForm() {
			return NormalBetaReduce(&e.Output)
		}
		err = BetaNormalError{e}
	case *Application:
		if !e.IsBetaNormalForm() {
			er := BetaReduce(expr)
			if er != nil {
				_, ok := er.(RedexError)
				if !ok {
					return er
				}
				if !e.Function.IsBetaNormalForm() {
					return NormalBetaReduce(&e.Function)
				}
				if !e.Argument.IsBetaNormalForm() {
					return NormalBetaReduce(&e.Argument)
				}
			}
			return

		}
		err = BetaNormalError{e}
	}
	return
}
