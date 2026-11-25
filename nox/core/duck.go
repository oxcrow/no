package core

import "fmt"

const (
	EUnknownError int = iota
	// Parser errors
	EStructInStruct
	EParseArrayExpectComma
	EParseStructExpectDot
	EParseStructExpectComma
)

func Duck(code int) []string {
	e, h := Buck(code)
	s := append(e, "")
	s = append(s, e...)
	s = append(s, "")
	s = append(s, h...)
	return s
}

func Buck(code int) ([]string, []string) {
	e := []string{}
	h := []string{}
	switch code {
	case EUnknownError:
		e = []string{
			Damn("Unable to uhh ... wait ... uhh. [UnknownError]"),
			"",
			Hint("Learn to speak, please."),
		}
	case EStructInStruct:
		e = []string{
			Damn("Unable to parse struct entity nested inside another struct."),
			"",
			Info("Members inside a struct must be variables or functions."),
			Info("Nested struct entities are not allowed."),
			"",
			Info("Such as,"),
			Line(1, "struct Missile {"),
			Line(2, "    location struct { // Allowed: Since this struct is a data member."),
			Line(3, "        ..."),
			Line(4, "    },"),
			Line(5, "    struct Location { // Error: Since this struct is an entity."),
			Line(6, "        ..."),
			Line(7, "    }"),
			Line(8, "}"),
		}
		h = []string{
			Hint("Either store the struct inside a variable,"),
			Hint("or outside as a separate struct."),
		}
	case EParseArrayExpectComma:
		e = []string{
			Damn("Unable to parse array as an expected comma is missing."),
			"",
			Info("Members inside an array must be separated by commas."),
			"",
			Info("Such as,"),
			Line(1, "primes = [4]int.{1, 2, 3, 5}"),
		}
		h = []string{}
	case EParseStructExpectDot:
		e = []string{
			Damn("Unable to parse array as an expected comma is missing."),
			"",
			Info("Members inside a struct must be initialized with dot."),
			"",
			Info("Such as,"),
			Line(1, "grumpy = Pet.{.legs = 3, .says = \"meow\"}"),
		}
		h = []string{}
	case EParseStructExpectComma:
		e = []string{
			Damn("Unable to parse struct as an expected comma is missing."),
			"",
			Info("Elements inside a struct must be separated by commas."),
			"",
			Info("Such as,"),
			Line(1, "bessie = Pet.{.legs = 4, .says = \"moo\"}"),
		}
		h = []string{}
	default:
		e = []string{
			Damn(fmt.Sprintf("Unknown error code: %d", code)),
		}
	}
	return e, h
}
