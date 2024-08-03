Imports System
Imports System.Text
Imports System.Collections.Generic

Module Program
    Sub Main(args As String())
        Console.WriteLine("Hello World!")
        Dim a As Object
        Dim e As New ExpressionEvaluator()
        a = e.EvaluatePostfix(e.InfixToPostfix("5+5"))
        Console.WriteLine(a)
        Console.ReadLine()
    End Sub
End Module

Public Class ExpressionEvaluator

    Private LookAhead As Tuple(Of String, String)
    Private ExprString As String
    Private LexPos As Integer

    ' Function to evaluate an expression in postfix notation
    Public Function EvaluatePostfix(expression As String) As Object
        Dim stack As New Stack(Of Object)()

        ' Split the expression by spaces
        Dim tokens As String() = expression.Split(" ")

        ' Loop through each token
        For Each token As String In tokens
            If IsNumeric(token) Then
                ' Push numbers onto the stack
                stack.Push(Convert.ToDouble(token))
            ElseIf token = "True" OrElse token = "False" Then
                ' Handle Boolean values
                stack.Push(If(token = "True", -1, 0))
            Else
                ' Pop operands for the operator
                Dim rightOperand As Double = Convert.ToDouble(stack.Pop())
                Dim leftOperand As Double = Convert.ToDouble(stack.Pop())

                ' Perform operation based on the operator
                Select Case token
                    Case "+"
                        stack.Push(leftOperand + rightOperand)
                    Case "-"
                        stack.Push(leftOperand - rightOperand)
                    Case "*"
                        stack.Push(leftOperand * rightOperand)
                    Case "/"
                        stack.Push(leftOperand / rightOperand)
                    Case "\"
                        stack.Push(Math.Floor(leftOperand / rightOperand))
                    Case "MOD"
                        stack.Push(leftOperand Mod rightOperand)
                    Case "^"
                        stack.Push(Math.Pow(leftOperand, rightOperand))
                    Case "="
                        stack.Push(If(leftOperand = rightOperand, -1, 0))
                    Case "<>"
                        stack.Push(If(leftOperand <> rightOperand, -1, 0))
                    Case ">"
                        stack.Push(If(leftOperand > rightOperand, -1, 0))
                    Case "<"
                        stack.Push(If(leftOperand < rightOperand, -1, 0))
                    Case ">="
                        stack.Push(If(leftOperand >= rightOperand, -1, 0))
                    Case "<="
                        stack.Push(If(leftOperand <= rightOperand, -1, 0))
                    Case "AND"
                        stack.Push(If(leftOperand <> 0 AndAlso rightOperand <> 0, -1, 0))
                    Case "OR"
                        stack.Push(If(leftOperand <> 0 OrElse rightOperand <> 0, -1, 0))
                    Case "XOR"
                        stack.Push(If(CBool(leftOperand) Xor CBool(rightOperand), -1, 0))
                    Case "EQV"
                        stack.Push(If(leftOperand = rightOperand, -1, 0))
                    Case "IMP"
                        stack.Push(If(leftOperand = 0 OrElse rightOperand <> 0, -1, 0))
                    Case "NOT"
                        stack.Push(If(rightOperand = 0, -1, 0))
                        ' Add cases for more logical operators if needed
                End Select
            End If
        Next

        ' The result is the only value left in the stack
        Return stack.Pop()
    End Function

    ' Function to convert an infix expression to postfix notation
    Public Function InfixToPostfix(expression As String) As String
        ExprString = expression
        LexPos = 0
        LookAhead = LexicalAnalyzer()

        Dim output As New StringBuilder()

        ' Call ParseExpression to convert infix to postfix
        ParseExpression(output)

        Return output.ToString().Trim()
    End Function

    ' Match current token and advance
    Private Sub MatchAndIncrement(checkValue As String)
        If LookAhead.Item1 = checkValue Then
            LookAhead = LexicalAnalyzer()
        Else
            Throw New Exception("Syntax Error")
        End If
    End Sub

    ' Parse expressions
    Private Sub ParseExpression(output As StringBuilder)
        ParseTerm(output)

        Do While LookAhead.Item1 = "+" OrElse LookAhead.Item1 = "-"
            Dim temp As String = LookAhead.Item1
            MatchAndIncrement(LookAhead.Item1)
            ParseTerm(output)
            AddToPostFix(output, temp)
        Loop
    End Sub

    ' Parse terms
    Private Sub ParseTerm(output As StringBuilder)
        ParseFactor(output)

        Do While LookAhead.Item1 = "*" OrElse LookAhead.Item1 = "/" OrElse LookAhead.Item1 = "MOD" OrElse LookAhead.Item1 = "\"
            Dim temp As String = LookAhead.Item1
            MatchAndIncrement(LookAhead.Item1)
            ParseFactor(output)
            AddToPostFix(output, temp)
        Loop
    End Sub

    ' Parse factors
    Private Sub ParseFactor(output As StringBuilder)
        Select Case LookAhead.Item1
            Case "("
                MatchAndIncrement("(")
                ParseExpression(output)
                MatchAndIncrement(")")
            Case "NUM"
                AddToPostFix(output, LookAhead.Item2)
                LookAhead = LexicalAnalyzer()
            Case "ID"
                AddToPostFix(output, LookAhead.Item2)
                LookAhead = LexicalAnalyzer()
            Case Else
                Throw New Exception("Syntax Error")
        End Select
    End Sub

    ' Add tokens to postfix output
    Private Sub AddToPostFix(output As StringBuilder, token As String)
        output.Append(token & " ")
    End Sub

    ' Function to simulate the lexical analyzer
    Private Function LexicalAnalyzer() As Tuple(Of String, String)
        ' Simulated lexical analysis for illustration
        If LexPos >= ExprString.Length Then
            Return Tuple.Create("", "")
        End If

        Dim currentChar As Char = ExprString(LexPos)
        LexPos += 1

        If Char.IsWhiteSpace(currentChar) Then
            Return LexicalAnalyzer()
        ElseIf Char.IsDigit(currentChar) Then
            Dim number As New StringBuilder(currentChar.ToString())
            While LexPos < ExprString.Length AndAlso Char.IsDigit(ExprString(LexPos))
                number.Append(ExprString(LexPos))
                LexPos += 1
            End While
            Return Tuple.Create("NUM", number.ToString())
        ElseIf Char.IsLetter(currentChar) Then
            Dim identifier As New StringBuilder(currentChar.ToString())
            While LexPos < ExprString.Length AndAlso Char.IsLetterOrDigit(ExprString(LexPos))
                identifier.Append(ExprString(LexPos))
                LexPos += 1
            End While
            Return Tuple.Create("ID", identifier.ToString())
        Else
            Return Tuple.Create(currentChar.ToString(), currentChar.ToString())
        End If
    End Function

    ' Function to look up numerical identifiers for trigonometric functions
    Private Function LookUpFunctionNumber(functionName As String) As Integer
        Select Case functionName.ToUpper()
            Case "SIN"
                Return 13
            Case "COS"
                Return 14
            Case "TAN"
                Return 15
            Case "SEC"
                Return 16
            Case "COSEC"
                Return 17
            Case "COT"
                Return 18
            Case "HSIN"
                Return 19
            Case "HCOS"
                Return 20
            Case "HTAN"
                Return 21
            Case "HSEC"
                Return 22
            Case "HCOSEC"
                Return 23
            Case "HCOT"
                Return 24
            Case Else
                Return 0
        End Select
    End Function

End Class
