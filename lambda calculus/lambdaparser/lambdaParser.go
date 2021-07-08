package lambdaparser

import (
	"fmt"
	"regexp"
	"strconv"
)

// LambdaSymbol is the representation of the Greek "lambda" symbol
var LambdaSymbol = "\\"

// PrettyLambda is the Greek "lambda" symbol
var PrettyLambda = "λ"

// LambdaParser holds the string to parse and the current location of the parser
type LambdaParser struct {
	ToParse     string
	CursorIndex int
	Current     byte
	Line        int
	Column      int
}

// SyntaxError error in lambda syntax
type SyntaxError struct {
	message string
	line    int
	column  int
}

// Variable is a string containing the variable name
type Variable struct {
	Name string
}

// Abstraction is a lambda function
type Abstraction struct {
	Param  Variable
	Output LambdaExpression
}

// Application is a lambda application
type Application struct {
	Function LambdaExpression
	Argument LambdaExpression
}

// LambdaExpression can be a pointer to a variable, an abstraction or an application
type LambdaExpression interface {
	String() string
	PrettyString() string
	GetFreeVariables() []*Variable
	IsBetaNormalForm() bool
}

func (v Variable) String() string {
	return fmt.Sprintf("(Var: {Name: %v})", v.Name)
}

// PrettyString returns the math string
func (v Variable) PrettyString() string {
	return v.Name
}

func (a Abstraction) String() string {
	return fmt.Sprintf("(Abstraction: {Param: %v, Output: %v})", a.Param, a.Output)
}

// PrettyString returns the math string
func (a Abstraction) PrettyString() string {
	return fmt.Sprintf("%v%v.%v", PrettyLambda, a.Param.PrettyString(), a.Output.PrettyString())
}

func (a Application) String() string {
	return fmt.Sprintf("(Application: {Function: %v, Argument: %v})", a.Function, a.Argument)
}

// PrettyString returns the math string
func (a Application) PrettyString() string {
	return fmt.Sprintf("(%v %v)", a.Function.PrettyString(), a.Argument.PrettyString())
}

func (e SyntaxError) Error() string {
	return fmt.Sprintf("SyntaxError: %v\nLine: %v, Column: %v", e.message, e.line, e.column)
}

func (p *LambdaParser) advance() {
	p.CursorIndex++
	p.Column++
	for !p.EOF() {
		p.Current = p.ToParse[p.CursorIndex]
		if isSpace(p.Current) {
			p.CursorIndex++
			p.Column++
		} else if p.Current == '\n' {
			p.CursorIndex++
			p.Line++
			p.Column = 0
		} else {
			break
		}
	}
}

func isSpace(s byte) bool {
	if s == ' ' || s == '\t' {
		return true
	}
	return false
}

// Init method initializes the LambdaParser with a string to parse
func (p *LambdaParser) Init(toParse string) {
	p.ToParse = toParse
	p.CursorIndex = -1
	p.Line = 1
	p.Column = -1
	p.advance()
	if !p.EOF() {
		p.Current = toParse[0]
	}
}

// EOF checks wether the end of the string has been reached
func (p LambdaParser) EOF() (isEOF bool) {
	if p.CursorIndex >= len(p.ToParse) {
		isEOF = true
	}
	return
}

// ParseVariable Parses variables that are of the form "[a-z]\d*"
func (p *LambdaParser) ParseVariable() (v Variable, err error) {
	if p.EOF() {
		err = SyntaxError{"Unexpected EOF", p.Line, p.Column}
		return
	}

	matchvar, er := regexp.Compile(`^[a-z]\d*`)
	if er != nil {
		err = er
		return
	}

	varname := matchvar.FindString(p.ToParse[p.CursorIndex:])

	if varname == "" {
		err = SyntaxError{"Illegal variable name", p.Line, p.Column}
		return
	}
	v.Name = varname

	p.CursorIndex += len(varname) - 1
	p.Column += len(varname) - 1
	p.advance()
	return
}

// ParseAbstraction parses abstractions (lambda functions)
func (p *LambdaParser) ParseAbstraction() (abstract Abstraction, err error) {
	if p.EOF() {
		err = SyntaxError{"Unexpected EOF", p.Line, p.Column}
		return
	}

	// Abstractions start with the "lambda" symbol
	if string(p.Current) != LambdaSymbol {
		err = SyntaxError{"Expected the lambda symbol in the abstraction", p.Line, p.Column}
		return
	}
	p.advance()

	if p.EOF() {
		err = SyntaxError{"Unexpected EOF", p.Line, p.Column}
		return
	}

	param, er := p.ParseVariable()
	if er != nil {
		err = er
		return
	}

	absParam := param

	if p.EOF() {
		err = SyntaxError{"Unexpected EOF", p.Line, p.Column}
		return
	}

	if p.Current != '.' {
		err = SyntaxError{"Expected a period in the abstraction", p.Line, p.Column}
		return
	}
	p.advance()

	if p.EOF() {
		err = SyntaxError{"Unexpected EOF", p.Line, p.Column}
		return
	}

	absOut, er := p.ParseExpression()
	if er != nil {
		err = er
		return
	}
	abstract.Param = absParam
	abstract.Output = absOut
	return
}

// ParseApplication parses lambda applications
func (p *LambdaParser) ParseApplication() (app Application, err error) {
	if p.EOF() {
		err = SyntaxError{"Unexpected EOF", p.Line, p.Column}
		return
	}

	if p.Current != '(' {
		err = SyntaxError{"Expected '('", p.Line, p.Column}
		return
	}
	p.advance()

	function, er := p.ParseExpression()
	if er != nil {
		err = er
		return
	}

	if p.EOF() {
		err = SyntaxError{"Unexpected EOF", p.Line, p.Column}
		return
	}

	arg, er := p.ParseExpression()
	if er != nil {
		err = er
		return
	}

	if p.EOF() {
		err = SyntaxError{"Unexpected EOF", p.Line, p.Column}
		return
	}

	if p.Current != ')' {
		err = SyntaxError{"Expected ')'", p.Line, p.Column}
		return
	}
	p.advance()
	app.Function = function
	app.Argument = arg

	return
}

// ParseExpression parses lambda expressions
// A lambda expression can be a variable, an abstraction or an application
func (p *LambdaParser) ParseExpression() (expression LambdaExpression, err error) {
	if p.EOF() {
		err = SyntaxError{"Unexpected EOF", p.Line, p.Column}
		return
	}

	isStart := p.CursorIndex == 0

	switch p.Current {
	case '(':
		{
			app, er := p.ParseApplication()
			if er != nil {
				err = er
				return
			}
			expression = &app
		}
	case '\\':
		{
			abs, er := p.ParseAbstraction()
			if er != nil {
				err = er
				return
			}
			expression = &abs
		}
	default:
		{
			vari, er := p.ParseVariable()
			if er != nil {
				err = er
				return
			}
			expression = &vari

		}
	}

	if isStart && !p.EOF() {
		err = SyntaxError{"Expected end of expression", p.Line, p.Column}
		return
	}

	return
}

// TryParseVariable parses string as lambda expression while dropping errors
func (p *LambdaParser) TryParseVariable(ToParse string) (v Variable) {
	(*p).Init(ToParse)
	v, err := (*p).ParseVariable()
	if err != nil {
		v = Variable{}
	}
	return
}

// TryParseAbstraction parses string as lambda expression while dropping errors
func (p *LambdaParser) TryParseAbstraction(ToParse string) (a Abstraction) {
	(*p).Init(ToParse)
	a, err := (*p).ParseAbstraction()
	if err != nil {
		a = Abstraction{}
	}
	return
}

// TryParseApplication parses string as lambda expression while dropping errors
func (p *LambdaParser) TryParseApplication(ToParse string) (a Application) {
	(*p).Init(ToParse)
	a, err := (*p).ParseApplication()
	if err != nil {
		a = Application{}
	}
	return
}

// TryParseExpression parses string as lambda expression while dropping errors
func (p *LambdaParser) TryParseExpression(ToParse string) (expr LambdaExpression) {
	(*p).Init(ToParse)
	expr, err := (*p).ParseExpression()
	if err != nil {
		expr = nil
	}
	return
}

// This shouldn't really be here, I should seperate this to a different file called
// something like "lambdaevaluate" and create a "lambdacommon" with all the definitions
// but I'm too lazy

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
		//freevars = append(freevars, *e)
		freevars[*e] = struct{}{}

	// The free variables of λx.M are the set of free variables of M, with x removed.
	case *Abstraction:
		//absfreevars := GetFreeVariablesNames(e.Output)
		//freevars = append(freevars, removeVariable(absfreevars, e.Param)...)
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
			//vars[len(vars)-1], vars[i] = vars[i], vars[len(vars)-1]
			//vars = vars[:len(vars)-1]
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
				// The param is in the free variables of With, so we need to alpha-convert In
				// such that the param won't be in the free variables of With anymore, so we can
				// just substitute the body (output)
				// e.AlphaConvert(freevarnames)
				// Substitute(Replace, With, &(*e).Output)
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
/*
func (a *Application) BetaReduce() (expr LambdaExpression, err error) {
	abs, ok := a.Function.(*Abstraction)
	if ok {
		err = Substitute(abs.Param, a.Argument, &abs.Output)
		expr = abs.Output
	} else {
		err = BetaNormalError{a}
	}
	return
}
*/

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
			err = Substitute(abs.Param, e.Argument, &abs.Output)
			if err == nil {
				*expr = abs.Output
			}
		} else {
			err = RedexError{e}
		}
	}
	return
}

/*
// ApplicativeBetaReduce - Take applicative order beta-reduction step
func (v *Variable) ApplicativeBetaReduce() (err error) {
	err = BetaNormalError{v}
	return
}

// ApplicativeBetaReduce - Take applicative order beta-reduction step
func (a *Abstraction) ApplicativeBetaReduce() (err error) {
	if a.IsBetaNormalForm() {
		return BetaNormalError{a}
	}
	return a.Output.ApplicativeBetaReduce()
}

// ApplicativeBetaReduce - Take applicative order beta-reduction step
// Leftnost innermost
func (a *Application) ApplicativeBetaReduce() (err error) {
	if a.IsBetaNormalForm() {
		return BetaNormalError{a}
	}
	if !a.Function.IsBetaNormalForm() {
		return a.Function.ApplicativeBetaReduce()
	}
	if !a.Argument.IsBetaNormalForm() {
		return a.Argument.ApplicativeBetaReduce()
	}
	return BetaReduce(a)
}
*/

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
