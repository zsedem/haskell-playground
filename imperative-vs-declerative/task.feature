Feature: Grep like command
   Scenario: Using the program I got sum of the numbers
      Given a file contains these numbers
        | numbers |
        | 0       |
        | 1       |
        | 2       |
       When I run the command
       Then I see the sum of these numbers on the stdout


