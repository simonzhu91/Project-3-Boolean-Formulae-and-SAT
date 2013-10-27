#!/usr/bin/env ruby

# tests boolean.ml for public inputs

tests = [ "assoc1", "assoc2", "boolean1", "boolean2", 
        "binary1", "binary2", "binary3", "binary4", 
        "magic1", "magic2", "magic3"]

tests.each do
        |test|

        puts "TESTING: #{test}"
        system("ocaml public_#{test}.ml")
end

