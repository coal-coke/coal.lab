#' GOST 25543 Qualification Data Base
#'
#' Compiled according to http://docs.cntd.ru/document/1200107843
#'
#' @noRd

fmt <- function(n)
  sprintf("%02i", n)

"%u%" <- function(s1, s2)
  as.vector(outer(s1, s2, paste0))

mk_code <- function(x){
  x[["code"]] <- do.call(
    function(Class, Category, Type, Subtype)
      fmt(Class) %u% Category %u% fmt(Type) %u% fmt(Subtype),
    x[["qualifier"]]
  )
  x
}

g25543db <- function() {
  all_classes <- 2:50
  all_categories <- 0:7
  all_types <- sort(unique(c(
    seq(10, 60, 10), seq(8, 48, 2),
    seq(5, 20, 5)
  )))
  all_subtypes <- sort(unique(c(seq(5, 20, 5), 0, 1, 6:70,
                                seq(20, 70, 10))))

  db <- list(
    # Brown ----
    brown = list(
      # begin brown coals
      # E, 1E =====
      G1Ex = list(
        id = c("\u0411", "1\u0411", NA, "E", "1E", NA),
        qualifier =
          list(
            Class = 2:3,
            Category = all_categories,
            Type = all_types[all_types >= 50],
            Subtype = c(5, 10, 15, 20)
          )

      ),

      # E, 2E, 2EB =====
      S2EBx = list(
        id = c("\u0411", "2\u0411", "2\u0411\u0412", "E",  "2E",  "2EB"),
        qualifier = list(
          Class = 2:4,
          Category = 0:3,
          Type = c(30, 40),
          Subtype = c(5, 10, 15, 20)
        )
      ),

      # E, 2E, 2EF =====
      S2EFx = list(
        id = c("\u0411", "2\u0411", "2\u0411\u0424", "E", "2E",  "2EF"),
        qualifier =
          list(
            Class = 2:4,
            Category = all_categories[all_categories >= 4],
            Type = c(30, 40),
            Subtype = c(5, 10, 15)
          )

      ),

      # E, 3E, 3EB =====
      S3EBx = list(
        id = c("\u0411", "3\u0411", "3\u0411\u0412", "E", "3E", "3EB"),
        qualifier = list(
          Class = 3:5,
          Category = 0:3,
          Type = c(10, 20),
          Subtype = c(5, 10, 15, 20)
        )
      ),

      # E, 3E, 3EF =====
      S3EFx = list(
        id = c("\u0411", "3\u0411", "3\u0411\u0424", "E",  "3E", "3EF"),
        qualifier =
          list(
            Class = 4:5,
            Category = all_categories[all_categories >= 4],
            Type = c(10, 20),
            Subtype = c(5, 10)
          )

      )
    ),
    # end brown coals

    # Hard ----
    hard = list(
      # begin hard coals
      # D, DB =====
      SDB04x = list(
        id = c("\u0414", NA, "\u0414\u0412", "D", NA, "DB"),
        qualifier = list(
          Class = 4,
          Category = 0:3,
          Type = all_types[all_types >= 40],
          Subtype = 0:1
        )
      ),

      SDB05x = list(
        id = c("\u0414", NA, "\u0414\u0412", "D", NA, "DB"),
        qualifier = list(
          Class = 5,
          Category = 0:3,
          Type = all_types[all_types >= 36],
          Subtype = 0:1
        )
      ),

      SDB06x = list(
        id = c("\u0414", NA, "\u0414\u0412", "D", NA, "DB"),
        qualifier = list(
          Class = 6,
          Category = 0:3,
          Type = all_types[all_types >= 34],
          Subtype = 0:1
        )
      ),

      SDB07x = list(
        id = c("\u0414", NA, "\u0414\u0412", "D", NA, "DB"),
        qualifier = list(
          Class = 7,
          Category = 0:3,
          Type = all_types[all_types >= 30],
          Subtype = 0:1
        )
      ),

      # D, DF =====
      SDF05x = list(
        id = c("\u0414", NA, "\u0414\u0424", "D", NA, "DF"),
        qualifier =
          list(
            Class = 5,
            Category = all_categories[all_categories >= 4],
            Type = all_types[all_types >= 30],
            Subtype = 0:1
          )

      ),

      SDF06x = list(
        id = c("\u0414", NA, "\u0414\u0424", "D", NA, "DF"),
        qualifier =
          list(
            Class = 6,
            Category = all_categories[all_categories >= 4],
            Type = all_types[all_types >= 28],
            Subtype = 0:1
          )

      ),

      SDF07x = list(
        id = c("\u0414", NA, "\u0414\u0424", "D", NA, "DF"),
        qualifier =
          list(
            Class = 7,
            Category = all_categories[all_categories >= 4],
            Type = all_types[all_types >= 30],
            Subtype = 0:1
          )

      ),

      # DG, DGB =====
      SDGBx = list(
        id = c("\u0414\u0413", NA, "\u0414\u0413\u0412", "DG", NA, "DGB"),
        qualifier = list(
          Class = 5:7,
          Category = 0:3,
          Type = all_types[all_types >= 32],
          Subtype = 6:9
        )
      ),

      # DG, DGF =====
      SDGFx = list(
        id = c("\u0414\u0413", NA, "\u0414\u0413\u0424", "DG", NA, "DGF"),
        qualifier =
          list(
            Class = 5:7,
            Category = all_categories[all_categories >= 4],
            Type = all_types[all_types >= 30],
            Subtype = 6:9
          )

      ),

      # G, 1G, 1GB =====
      S1GB5x = list(
        id = c("\u0413", "1\u0413", "1\u0413\u0412", "G", "1G", "1GB"),
        qualifier =
          list(
            Class = 5:8,
            Category = 0:3,
            Type = all_types[all_types >= 38],
            Subtype = 10:12
          )

      ),

      S1GB8x = list(
        id = c("\u0413", "1\u0413", "1\u0413\u0412", "G", "1G", "1GB"),
        qualifier = list(
          Class = 8:9,
          Category = 0:3,
          Type = all_types[all_types >= 30],
          Subtype = 6:9
        )
      ),

      # G, 1G, 1GF =====
      S1GF5x = list(
        id = c("\u0413", "1\u0413", "1\u0413\u0424", "G",  "1G", "1GF"),
        qualifier =
          list(
            Class = 5,
            Category = all_categories[all_categories >= 4],
            Type = all_types[all_types >= 30],
            Subtype = 10:12
          )

      ),

      S1GF6x = list(
        id = c("\u0413", "1\u0413", "1\u0413\u0424", "G",  "1G", "1GF"),
        qualifier =
          list(
            Class = 6:7,
            Category = all_categories[all_categories >= 4],
            Type = all_types[all_types >= 38],
            Subtype = 10:12
          )

      ),

      S1GF8x = list(
        id = c("\u0413", "1\u0413", "1\u0413\u0424", "G",  "1G", "1GF"),
        qualifier =
          list(
            Class = 8:9,
            Category = all_categories[all_categories >= 4],
            Type = all_types[all_types >= 30],
            Subtype = 6:9
          )

      ),

      # G, 2G =====
      G2Gx = list(
        id = c("\u0413", "2\u0413", NA, "G",  "2G", NA),
        qualifier =
          list(
            Class = 6:7,
            Category = all_categories,
            Type = all_types[all_types >= 38],
            Subtype = 13:16
          )

      ),

      # GXO, 1GXO, 1GXOB =====
      S1GXOBx = list(
        id = c(
          "\u0413\u0416\u041E",
          "1\u0413\u0416\u041E",
          "1\u0413\u0416\u041E\u0412",
          "GXO",
          "1GXO",
          "1GXOB"
        ),
        qualifier = list(
          Class = 6:7,
          Category = 0:3,
          Type = c(30, 32, 34, 36),
          Subtype = 10:16
        )
      ),

      # GXO, 1GXO, 1GXOF =====
      S1GXOFx = list(
        id = c(
          "\u0413\u0416\u041E",
          "1\u0413\u0416\u041E",
          "1\u0413\u0416\u041E\u0424",
          "GXO",
          "1GXO",
          "1GXOF"
        ),
        qualifier =
          list(
            Class = 6:7,
            Category = all_categories[all_categories >= 4],
            Type = seq(30, 36, 2),
            Subtype = 10:16
          )

      ),

      # GXO, 2GXO, 2GXOB =====
      S2GXOB30x = list(
        id = c(
          "\u0413\u0416\u041E",
          "2\u0413\u0416\u041E",
          "2\u0413\u0416\u041E\u0412",
          "GXO",
          "2GXO",
          "2GXOB"
        ),
        qualifier = list(
          Class = 8:9,
          Category = 0:3,
          Type = seq(30, 36, 2),
          Subtype = 10:13
        )
      ),

      S2GXOB36x = list(
        id = c(
          "\u0413\u0416\u041E",
          "2\u0413\u0416\u041E",
          "2\u0413\u0416\u041E\u0412",
          "GXO",
          "2GXO",
          "2GXOB"
        ),
        qualifier = list(
          Class = 8,
          Category = 0:3,
          Type = all_types[all_types >= 36],
          Subtype = 14:16
        )
      ),

      # GXO, 2GXO, 2GXOF =====
      S2GXOF30x = list(
        id = c(
          "\u0413\u0416\u041E",
          "2\u0413\u0416\u041E",
          "2\u0413\u0416\u041E\u0424",
          "GXO",
          "2GXO",
          "2GXOF"
        ),
        qualifier =
          list(
            Class = 8:9,
            Category = all_categories[all_categories >= 4],
            Type = all_types[all_types >= 30],
            Subtype = 10:13
          )

      ),
      S2GXOF36x = list(
        id = c(
          "\u0413\u0416\u041E",
          "2\u0413\u0416\u041E",
          "2\u0413\u0416\u041E\u0424",
          "GXO",
          "2GXO",
          "2GXOF"
        ),
        qualifier =
          list(
            Class = 8,
            Category = all_categories[all_categories >= 4],
            Type = all_types[all_types >= 36],
            Subtype = 14:16
          )

      ),

      # GX, 1GX =====
      G1GXx = list(
        id = c("\u0413\u0416", "1\u0413\u0416", NA, "GX", "1GX", NA),
        qualifier =
          list(
            Class = 5:7,
            Category = all_categories,
            Type = all_types[all_types >= 30],
            Subtype = all_subtypes[all_subtypes >= 17]
          )

      ),

      # GX, 2GX =====
      G2GXx = list(
        id = c("\u0413\u0416", "2\u0413\u0416", NA, "GX",  "2GX", NA),
        qualifier =
          list(
            Class = 8:9,
            Category = all_categories,
            Type = all_types[all_types >= 36],
            Subtype = 17:25
          )

      ),

      # X, 1X =====
      G1X8x = list(
        id = c("\u0416", "1\u0416", NA, "X", "1X", NA),
        qualifier =
          list(
            Class = 8,
            Category = all_categories,
            Type = seq(28, 34, 2),
            Subtype = 14:17
          )

      ),

      G1X9x = list(
        id = c("\u0416", "1\u0416", NA, "X",  "1X", NA),
        qualifier =
          list(
            Class = 9:11,
            Category = all_categories,
            Type = c(30, 32, 34),
            Subtype = 14:17
          )

      ),

      # X, 2X =====
      G2X81x = list(
        id = c("\u0416", "2\u0416", NA, "X",  "2X", NA),
        qualifier =
          list(
            Class = 8:9,
            Category = all_categories,
            Type = all_types[all_types >= 36],
            Subtype = all_subtypes[all_subtypes >= 26]
          )

      ),

      G2X82x = list(
        id = c("\u0416", "2\u0416", NA, "X",  "2X", NA),
        qualifier =
          list(
            Class = 8:9,
            Category = all_categories,
            Type = c(30, 32, 34),
            Subtype = all_subtypes[all_subtypes >= 18]
          )

      ),

      G2X10x = list(
        id = c("\u0416", "2\u0416", NA, "X",  "2X", NA),
        qualifier =
          list(
            Class = 10:11,
            Category = all_categories,
            Type = all_types[all_types >= 30],
            Subtype = all_subtypes[all_subtypes >= 18]
          )

      ),

      # KX =====
      MKXx = list(
        id = c("\u041A\u0416", NA, NA, "KX", NA, NA),
        qualifier =
          list(
            Class = 9:12,
            Category = all_categories,
            Type = c(24, 26, 28),
            Subtype = all_subtypes[all_subtypes >= 18]
          )

      ),

      # K, 1K, 1KB =====
      S1KB1x = list(
        id = c("\u041A", "1\u041A", "1\u041A\u0412", "K", "1K", "1KB"),
        qualifier = list(
          Class = 10:12,
          Category = 0:3,
          Type = c(24, 26, 28),
          Subtype = 13:17
        )
      ),

      S1KB2x = list(
        id = c("\u041A", "1\u041A", "1\u041A\u0412", "K", "1K", "1KB"),
        qualifier =
          list(
            Class = 10:12,
            Category = 0:3,
            Type = all_types[all_types < 24],
            Subtype = all_subtypes[all_subtypes >= 13]
          )

      ),

      # K, 1K, 1KF =====
      S1KF1x = list(
        id = c("\u041A", "1\u041A", "1\u041A\u0424", "K", "1K", "1KF"),
        qualifier =
          list(
            Class = 10:12,
            Category = all_categories[all_categories >= 4],
            Type = c(24, 26, 28),
            Subtype = 13:17
          )

      ),

      S1KF2x = list(
        id = c("\u041A", "1\u041A", "1\u041A\u0424", "K", "1K", "1KF"),
        qualifier =
          list(
            Class = 10:12,
            Category = all_categories[all_categories >= 4],
            Type = all_types[all_types < 24],
            Subtype = all_subtypes[all_subtypes >= 13]
          )

      ),

      # К, 2K, 2KB =====
      S2KB1x = list(
        id = c("\u041A", "2\u041A", "2\u041A\u0412", "K", "2K", "2KB"),
        qualifier =
          list(
            Class = 13:16,
            Category = 0:3,
            Type = all_types[all_types <= 28],
            Subtype = all_subtypes[all_subtypes >= 13]
          )

      ),

      # S2KB2x = list(
      #   id = c("\u041A", "2\u041A", "2\u041A\u0412", "K", "2K", "2KB"),
      #   qualifier = list(Class = all_classes[all_classes >= 14],
      #                    Category = 0:3,
      #                    Type = all_types[all_types <= 28],
      #                    Subtype = all_subtypes[all_subtypes < 13]))),

      # K, 2K, 2KF =====
      S2KFx = list(
        id = c("\u041A", "2\u041A", "2\u041A\u0424", "K", "2K", "2KF"),
        qualifier =
          list(
            Class = 13:16,
            Category = all_categories[all_categories >= 4],
            Type = all_types[all_types <= 28],
            Subtype = all_subtypes[all_subtypes >= 13]
          )

      ),

      # KO, 1KO, 1KOB =====
      S1KOBx = list(
        id = c(
          "\u041A\u041E",
          "1\u041A\u041E",
          "1\u041A\u041E\u0412",
          "KO",
          "1KO",
          "1KOB"
        ),
        qualifier = list(
          Class = 8:11,
          Category = 0:3,
          Type = c(22, 24, 26, 28),
          Subtype = 10:12
        )
      ),

      # KO, 1KO, 1KOF =====
      S1KOF1x = list(
        id = c(
          "\u041A\u041E",
          "1\u041A\u041E",
          "1\u041A\u041E\u0424",
          "KO",
          "1KO",
          "1KOF"
        ),
        qualifier =
          list(
            Class = 8:9,
            Category = all_categories[all_categories >= 4],
            Type = c(22, 24, 26, 28),
            Subtype = 10:12
          )

      ),

      S1KOF2x = list(
        id = c(
          "\u041A\u041E",
          "1\u041A\u041E",
          "1\u041A\u041E\u0424",
          "KO",
          "1KO",
          "1KOF"
        ),
        qualifier =
          list(
            Class = 10:11,
            Category = all_categories[all_categories >= 4],
            Type = all_types[all_types >= 20],
            Subtype = 10:12
          )

      ),

      # KO, 2KO, 2KOB =====
      S2KOB11x = list(
        id = c(
          "\u041A\u041E",
          "2\u041A\u041E",
          "2\u041A\u041E\u0412",
          "KO",
          "2KO",
          "2KOB"
        ),
        qualifier = list(
          Class = 11,
          Category = 0:3,
          Type = c(16, 18, 20),
          Subtype = 10:12
        )
      ),

      S2KOB12x = list(
        id = c(
          "\u041A\u041E",
          "2\u041A\u041E",
          "2\u041A\u041E\u0412",
          "KO",
          "2KO",
          "2KOB"
        ),
        qualifier = list(
          Class = 12,
          Category = 0:3,
          Type = all_types[all_types <= 28],
          Subtype = 10:12
        )
      ),

      S2KOB13x = list(
        id = c(
          "\u041A\u041E",
          "2\u041A\u041E",
          "2\u041A\u041E\u0412",
          "KO",
          "2KO",
          "2KOB"
        ),
        qualifier = list(
          Class = 13,
          Category = 0:3,
          Type = c(22, 24, 26),
          Subtype = 10:12
        )
      ),

      # KO, 2KO, 2KOF =====
      S2KOF11x = list(
        id = c(
          "\u041A\u041E",
          "2\u041A\u041E",
          "2\u041A\u041E\u0424",
          "KO",
          "2KO",
          "2KOF"
        ),
        qualifier =
          list(
            Class = 11,
            Category = all_categories[all_categories >= 4],
            Type = c(16, 18),
            Subtype = 10:12
          )

      ),

      S2KOF12x = list(
        id = c(
          "\u041A\u041E",
          "2\u041A\u041E",
          "2\u041A\u041E\u0424",
          "KO",
          "2KO",
          "2KOF"
        ),
        qualifier =
          list(
            Class = 12,
            Category = all_categories[all_categories >= 4],
            Type = all_types[all_types <= 28],
            Subtype = 10:12
          )

      ),

      S2KOF13x = list(
        id = c(
          "\u041A\u041E",
          "2\u041A\u041E",
          "2\u041A\u041E\u0424",
          "KO",
          "2KO",
          "2KOF"
        ),
        qualifier =
          list(
            Class = 13,
            Category = all_categories[all_categories >= 4],
            Type = c(22, 24, 26),
            Subtype = 10:12
          )

      ),

      # KCH, KCHB =====
      SKCHBx = list(
        id = c(
          "\u041A\u0421\u041D",
          NA,
          "\u041A\u0421\u041D\u0412",
          "KCH",
          NA,
          "KCHB"
        ),
        qualifier = list(
          Class = 8:10,
          Category = 0:3,
          Type = all_types[all_types <= 28],
          Subtype = 6:9
        )
      ),

      # KCH, KCHF =====
      SKCHFx = list(
        id = c(
          "\u041A\u0421\u041D",
          NA,
          "\u041A\u0421\u041D\u0424",
          "KCH",
          NA,
          "KCHF"
        ),
        qualifier =
          list(
            Class = 8:10,
            Category = all_categories[all_categories >= 4],
            Type = all_types[all_types <= 28],
            Subtype = 6:9
          )

      ),
      # KC, 1KC, 1KCB =====
      S1KCBx = list(
        id = c(
          "\u041A\u0421",
          "1\u041A\u0421",
          "1\u041A\u0421\u0412",
          "KC",
          "1KC",
          "1KCB"
        ),
        qualifier =
          list(
            Class = 11:13,
            Category = 0:3,
            Type = all_types[all_types <= 28],
            Subtype = 6:9
          )

      ),

      # KC, 1KC, 1KCF =====
      S1KCFx = list(
        id = c(
          "\u041A\u0421",
          "1\u041A\u0421",
          "1\u041A\u0421\u0424",
          "KC",
          "1KC",
          "1KCF"
        ),
        qualifier =
          list(
            Class = 11:13,
            Category = all_categories[all_categories >= 4],
            Type = all_types[all_types <= 28],
            Subtype = 6:9
          )

      ),

      # KC, 2KC, 2KCB =====
      S2KCB14x = list(
        id = c(
          "\u041A\u0421",
          "2\u041A\u0421",
          "2\u041A\u0421\u0412",
          "KC",
          "2KC",
          "2KCB"
        ),
        qualifier = list(
          Class = 14,
          Category = 0:3,
          Type = all_types[all_types <= 24],
          Subtype = 6:9
        )
      ),

      S2KCB15x = list(
        id = c(
          "\u041A\u0421",
          "2\u041A\u0421",
          "2\u041A\u0421\u0412",
          "KC",
          "2KC",
          "2KCB"
        ),
        qualifier =
          list(
            Class = 15:16,
            Category = 0:3,
            Type = all_types[all_types <= 24],
            Subtype = 6:8
          )

      ),

      # KC, 2KC, 2KCF =====
      S2KCFx = list(
        id = c(
          "\u041A\u0421",
          "2\u041A\u0421",
          "2\u041A\u0421\u0424",
          "KC",
          "2KC",
          "2KCF"
        ),
        qualifier =
          list(
            Class = 14:16,
            Category = all_categories[all_categories >= 4],
            Type = all_types[all_types <= 24],
            Subtype = 6:9
          )

      ),

      # OC, 1OC, 1OCB =====
      S1OCB13x = list(
        id = c(
          "\u041E\u0421",
          "1\u041E\u0421",
          "1\u041E\u0421\u0412",
          "OC",
          "1OC",
          "1OCB"
        ),
        qualifier =
          list(
            Class = 13:14,
            Category = 0:3,
            Type = all_types[all_types <= 20],
            Subtype = 10:12
          )

      ),

      S1OCB15x = list(
        id = c(
          "\u041E\u0421",
          "1\u041E\u0421",
          "1\u041E\u0421\u0412",
          "OC",
          "1OC",
          "1OCB"
        ),
        qualifier =
          list(
            Class = 15:16,
            Category = 0:3,
            Type = all_types[all_types <= 20],
            Subtype = 9:12
          )

      ),

      S1OCB17x = list(
        id = c(
          "\u041E\u0421",
          "1\u041E\u0421",
          "1\u041E\u0421\u0412",
          "OC",
          "1OC",
          "1OCB"
        ),
        qualifier = list(
          Class = 17,
          Category = 0:3,
          Type = all_types[all_types <= 20],
          Subtype = 10:12
        )
      ),

      # OC, 1OC, 1OCF =====
      S1OCFx = list(
        id = c(
          "\u041E\u0421",
          "1\u041E\u0421",
          "1\u041E\u0421\u0424",
          "OC",
          "1OC",
          "1OCF"
        ),
        qualifier =
          list(
            Class = 13:17,
            Category = all_categories[all_categories >= 4],
            Type = all_types[all_types <= 20],
            Subtype = 10:12
          )

      ),

      # OC, 2OC, 2OCB =====
      S2OCBx = list(
        id = c(
          "\u041E\u0421",
          "2\u041E\u0421",
          "2\u041E\u0421\u0412",
          "OC",
          "2OC",
          "2OCB"
        ),
        qualifier =
          list(
            Class = all_classes[all_classes >= 17],
            Category = 0:3,
            Type = all_types[all_types <= 20],
            Subtype = 6:9
          )

      ),

      # OC, 2OC, 2OCF =====
      S2OCFx = list(
        id = c(
          "\u041E\u0421",
          "2\u041E\u0421",
          "2\u041E\u0421\u0424",
          "OC",
          "2OC",
          "2OCF"
        ),
        qualifier =
          list(
            Class = all_classes[all_classes >= 17],
            Category = all_categories[all_categories >= 4],
            Type = all_types[all_types <= 20],
            Subtype = 6:9
          )

      ),

      # TC, TCB =====
      STCBx = list(
        id = c("\u0422\u0421", NA, "\u0422\u0421\u0412", "TC", NA, "TCB"),
        qualifier = list(
          Class = 14:19,
          Category = 0:3,
          Type = all_types[all_types <= 20],
          Subtype = 1
        )
      ),

      # TC, TCF =====
      STCF14x = list(
        id = c("\u0422\u0421", NA, "\u0422\u0421\u0424", "TC", NA, "TCF"),
        qualifier =
          list(
            Class = 14:15,
            Category = all_categories[all_categories >= 4],
            Type = 16:18,
            Subtype = 1
          )

      ),

      STCF16x = list(
        id = c("\u0422\u0421", NA, "\u0422\u0421\u0424", "TC", NA, "TCF"),
        qualifier =
          list(
            Class = 16:19,
            Category = all_categories[all_categories >= 4],
            Type = all_types[all_types <= 16],
            Subtype = 1
          )

      ),

      # CC, 1CC =====
      G1CC7x = list(
        id = c("\u0421\u0421", "1\u0421\u0421", NA, "CC", "1CC", NA),
        qualifier = list(
          Class = 7,
          Category = all_categories,
          Type = c(20, 22, 24, 26, 28),
          Subtype = 0:1
        )
      ),

      G1CC8x = list(
        id = c("\u0421\u0421", "1\u0421\u0421", NA, "CC", "1CC", NA),
        qualifier =
          list(
            Class = 8:9,
            Category = all_categories,
            Type = all_types[all_types >= 34],
            Subtype = 0:1
          )

      ),
      # CC, 2CC =====
      G2CCx = list(
        id = c("\u0421\u0421", "2\u0421\u0421", NA, "CC", "2CC", NA),
        qualifier =
          list(
            Class = 8:13,
            Category = all_categories,
            Type = c(26, 28, 30, 32),
            Subtype = 0:1
          )

      ),

      # CC, 3CC =====
      G3CC08x = list(
        id = c("\u0421\u0421", "3\u0421\u0421", NA, "CC", "3CC", NA),
        qualifier =
          list(
            Class = 8:9,
            Category = all_categories,
            Type = c(20, 22, 24),
            Subtype = 0:1
          )

      ),
      G3CC10x = list(
        id = c("\u0421\u0421", "3\u0421\u0421", NA, "CC", "3CC", NA),
        qualifier =
          list(
            Class = 10:13,
            Category = all_categories,
            Type = c(16, 18, 20, 22, 24),
            Subtype = 0:1
          )

      ),

      G3CC14x = list(
        id = c("\u0421\u0421", "3\u0421\u0421", NA, "CC", "3CC", NA),
        qualifier = list(
          Class = 14,
          Category = all_categories,
          Type = c(16, 18, 20),
          Subtype = 0
        )
      ),

      G3CC15x = list(
        id = c("\u0421\u0421", "3\u0421\u0421", NA, "CC", "3CC", NA),
        qualifier =
          list(
            Class = 15:17,
            Category = all_categories,
            Type = c(18, 20),
            Subtype = 0
          )

      ),

      # T, 1T, 1TB =====
      S1TBx = list(
        id = c("\u0422", "1\u0422", "1\u0422\u0412", "T", "1T", "1TB"),
        qualifier = list(
          Class = 15:20,
          Category = 0:3,
          Type = c(12, 14, 16),
          Subtype = 0
        )
      ),

      # T, 1T, 1TF =====
      S1TFx = list(
        id = c("\u0422", "1\u0422", "1\u0422\u0424", "T", "1T", "1TF"),
        qualifier =
          list(
            Class = 13:20,
            Category = all_categories[all_categories >= 4],
            Type = c(12, 14),
            Subtype = 0
          )

      ),

      # T, 2T, 2TB =====
      S2TBx = list(
        id = c("\u0422", "2\u0422", "1\u0422\u0412", "T", "2T", "2TB"),
        qualifier = list(
          Class = 15:25,
          Category = 0:3,
          Type = c(8, 10),
          Subtype = 0
        )
      ),

      #  T, 2T, 2TF =====
      S2TFx = list(
        id = c("\u0422", "2\u0422", "2\u0422\u0424", "T", "2T", "2TF"),
        qualifier =
          list(
            Class = 15:25,
            Category = all_categories[all_categories >= 4],
            Type = c(8, 10),
            Subtype = 0
          )

      )
    ),
    # end of hard coals

    # Anthracite ----
    anthracite = list(
      # begin anthracite coals
      # A, 1A, 1AB =====
      S1ABx = list(
        id = c("\u0410", "1\u0410", "1\u0410\u0412", "A", "1A", "1AB"),
        qualifier =
          list(
            Class = 22:35,
            Category = 0:3,
            Type = 20,
            Subtype = all_subtypes[all_subtypes <= 60]
          )

      ),

      # A, 1A, 1AF =====
      S1AFx = list(
        id = c("\u0410", "1\u0410", "1\u0410\u0424", "A", "1A", "1AF"),
        qualifier =
          list(
            Class = 22:35,
            Category = all_categories[all_categories >= 4],
            Type = all_types[all_types >= 10],
            Subtype = all_subtypes[all_subtypes <= 60]
          )

      ),

      # A, 2A, 2AB =====
      S2ABx = list(
        id = c("\u0410", "2\u0410", "2\u0410\u0412", "A", "2A", "2AB"),
        qualifier =
          list(
            Class = 36:44,
            Category = 0:3,
            Type = all_types[all_types >= 10],
            Subtype = all_subtypes[all_subtypes >= 40]
          )

      ),

      # A, 2A, 2AF =====
      S2AFx = list(
        id = c("\u0410", "2\u0410", "2\u0410\u0424", "A", "2A", "2AF"),
        qualifier =
          list(
            Class = 36:44,
            Category = all_categories[all_categories >= 4],
            Type = all_types[all_types >= 10],
            Subtype = all_subtypes[all_subtypes >= 40]
          )

      ),

      # A, 3A, 3AB =====
      S3ABx = list(
        id = c("\u0410", "3\u0410", "3\u0410\u0412", "A", "3A", "3AB"),
        qualifier =
          list(
            Class = all_classes[all_classes >= 45],
            Category = 0:3,
            Type = all_types[all_types <= 15],
            Subtype = all_subtypes[all_subtypes >= 50]
          )

      ),

      # A, 3A, 3AF =====
      S3AFx = list(
        id = c("\u0410", "3\u0410", "3\u0410\u0424", "A", "3A", "3AF"),
        qualifier =
          list(
            Class = all_classes[all_classes >= 45],
            Category = all_categories[all_categories >= 4],
            Type = all_types[all_types <= 15],
            Subtype = all_subtypes[all_subtypes >= 50]
          )

      )
    ) # end anthracite coals
  )

  db[["brown"]] <- lapply(db[["brown"]], mk_code)
  db[["hard"]]  <- lapply(db[["hard"]],  mk_code)
  db[["anthracite"]] <- lapply(db[["anthracite"]], mk_code)
  db
}
