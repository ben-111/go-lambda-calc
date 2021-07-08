package lambda

import (
	"fmt"
	"regexp"
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
