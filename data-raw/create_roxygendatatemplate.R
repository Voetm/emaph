a = "\\describe{\n"

for(n in names(d)) {
  a = paste(a,
            sprintf("  \\item{\\code{%s}}{%s}",
                    n, attributes(d[[n]])$label), sep = "\n")
}
paste(a, "}", sep = "\n")
cat(a)


