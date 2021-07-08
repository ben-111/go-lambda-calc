package main

import (
	"flag"
	"fmt"
	"os"

	"example.com/lambda"
)

type flagsS struct {
	expr          string
	isNormalOrder bool
}

var flags flagsS

func init() {
	flag.BoolVar(&flags.isNormalOrder, "normal", false, "Will use normal order reduction strategy instead of applicative order.")
}

func main() {
	flag.Parse()
	flags.expr = flag.Arg(0)
	if flags.expr == "" {
		fmt.Fprintf(os.Stderr, "Usage: %s [--normal] LambdaExpression\n", os.Args[0])
		flag.PrintDefaults()
		fmt.Println("  -LambdaExpression\n\tThe Lambda expression to parse. Use '\\' for Lambda.")
		fmt.Printf("\nExample:\n\t%s --normal \"(((((\\x.\\y.\\z.z y) x) \\x.(x x)) \\x.x) x)\"\n", os.Args[0])
		return
	}
	var p lambda.LambdaParser
	p.Init(flags.expr)
	expr, err := p.ParseExpression()
	if err != nil {
		fmt.Println(err)
		return
	}
	order := "Applicative"
	if flags.isNormalOrder {
		order = "Normal"
	}
	fmt.Printf("Using %v Order reduction.\n", order)
	fmt.Printf("Lambda expression: %v\n", expr.PrettyString())

	shell(expr)
}

func shell(expr lambda.LambdaExpression) {
	index := 1
	for ; ; index++ {
		if index == 1 {
			fmt.Printf("Start. . . ")
		} else {
			fmt.Printf("Continue. . . ")
		}

		_, err := fmt.Scanln()
		if err != nil {
			var discard string
			fmt.Scanln(&discard)
		}

		err1 := step(&expr, index)
		if err1 != nil {
			return
		}
	}
}

func step(expr *lambda.LambdaExpression, index int) error {
	var err error
	if flags.isNormalOrder {
		err = lambda.NormalBetaReduce(expr)
	} else {
		err = lambda.ApplicativeBetaReduce(expr)
	}

	if err != nil {
		switch et := err.(type) {
		case lambda.BetaNormalError:
			fmt.Printf("\nExpression is in Beta-Normal form!\n\n")
			return et
		case lambda.SubstitutionError:
			fmt.Printf("    %v. %v => ", index, (*expr).PrettyString())
			et.Convert.AlphaConvert(et.Avoid)
			fmt.Printf("%v\n", (*expr).PrettyString())
			return nil
		}
	}
	fmt.Printf("    %v. %v\n", index, (*expr).PrettyString())
	return nil
}
