list(
## Tier 1
    list(
        Response_Cries = c("\\boh", "\\bah", "\\baha", "\\bouch", "yuk"),
        Back_Channels = c("uh[- ]huh", "uhuh", "yeah"),
        Summons = "hey",
        Justification = "because"
    ),

 ## Tier 2
    list(
        Summons = 'the',
        Back_Channels = c('\\ber+\\b', 'hm+'),
        Empty = c('^\\s*$')
    ),

 ## Tier 3
    list(
        Summons = c(),
        Back_Channels = c()
    ),

## Tier 4
    list(
        Summons = 'it',
        Summons = 'hey you',
        Justification = 'ed\\s',
        Exclamation = c('\\!$', '^[^w]{0,5}wow+.{0,4}'),
        Empty = c()
    )
)
