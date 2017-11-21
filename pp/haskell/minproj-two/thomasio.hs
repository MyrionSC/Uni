main :: IO ()
main = writeOutput "output.tex" tests
--main = execLaTeXT output >>= renderFile "output.tex"

writeOutput :: FilePath -> String -> IO ()
writeOutput path contents = do
  handle <- openFile path WriteMode
  mapM_ (hPutStrLn handle) [ "\\documentclass[12pt]{article}"
                      , "\\usepackage[utf8]{inputenc}"
                      , "\\usepackage{amsmath}"
                      , "\\usepackage{amsfonts}"
                      , "\\usepackage{amssymb}"
                      , "\\usepackage{graphicx}"
                      , "\\begin{document}"
                      , "\\section{Haskell Miniproject Test Cases}"
                      , "\\makebox[\\textwidth][c]{\\scalebox{1.5}{"
                      , "\\begin{tabular}{| l | c | r |}"
                      , "\\hline"
                      , "\\textbf{Test number} & \\textbf{Input/Output} & \\textbf{Result} \\\\ \\hline "
                      , contents ++ "\\\\"
                      , "\\hline"
                      , "\\end{tabular}}}"
                      , "\\end{document}"
                      ]
  hClose handle
