%% -*- mode: LaTeX; compile-command: "./Shake.hs" -*-

\documentclass[xcolor=svgnames,12pt,aspectratio=169]{beamer}

%include polycode.fmt

\usepackage{xspace}
\usepackage{ulem}
\usepackage{qtree}
\usepackage{graphicx}
\graphicspath{{images/}}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\newcommand{\etc}{\textit{etc.}}
\newcommand{\eg}{\textit{e.g.}\xspace}
\newcommand{\ie}{\textit{i.e.}\xspace}

\newcommand{\thevenue}{Haskell Symposium}
\newcommand{\thedate}{6 September 2024}

\newcounter{langcounter}
\newcommand{\lang}[1]{\stepcounter{langcounter}{\Large \arabic{langcounter}. #1}}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\setbeamertemplate{items}[circle]

\mode<presentation>
{
  \usetheme{default}                          % use a default (plain) theme

  \setbeamertemplate{navigation symbols}{}    % don't show navigation
                                              % buttons along the
                                              % bottom
  \setbeamerfont{normal text}{family=\sffamily}

  % XX remove this before giving actual talk!
  % \setbeamertemplate{footline}[frame number]
  % {%
  %   \begin{beamercolorbox}{section in head/foot}
  %     \vskip2pt
  %     \hfill \insertframenumber
  %     \vskip2pt
  %   \end{beamercolorbox}
  % }

  \AtBeginSection[]
  {
    \begin{frame}<beamer>
      \frametitle{}

      \begin{center}
        \includegraphics[width=3in]{\sectionimg}
        \bigskip

        {\Huge \usebeamercolor[fg]{title}\insertsectionhead}
      \end{center}
    \end{frame}
  }
}

\newcommand{\sectionimg}{}

\defbeamertemplate*{title page}{customized}[1][]
{
  \vbox{}
  \vfill
  \begin{centering}
    \begin{beamercolorbox}[sep=8pt,center,#1]{title}
      \usebeamerfont{title}\inserttitle\par%
      \ifx\insertsubtitle\@@empty%
      \else%
        \vskip0.25em%
        {\usebeamerfont{subtitle}\usebeamercolor[fg]{subtitle}\insertsubtitle\par}%
      \fi%
    \end{beamercolorbox}%
    \vskip1em\par
    {\usebeamercolor[fg]{titlegraphic}\inserttitlegraphic\par}
    \vskip1em\par
    \begin{beamercolorbox}[sep=8pt,center,#1]{author}
      \usebeamerfont{author}\insertauthor
    \end{beamercolorbox}
    \begin{beamercolorbox}[sep=8pt,center,#1]{institute}
      \usebeamerfont{institute}\insertinstitute
    \end{beamercolorbox}
    \begin{beamercolorbox}[sep=8pt,center,#1]{date}
      \usebeamerfont{date}\insertdate
    \end{beamercolorbox}
  \end{centering}
  \vfill
}

\newenvironment{xframe}[1][]
  {\begin{frame}[fragile,environment=xframe,#1]}
  {\end{frame}}

% uncomment me to get 4 slides per page for printing
% \usepackage{pgfpages}
% \pgfpagesuselayout{4 on 1}[uspaper, border shrink=5mm]

\setbeameroption{show notes}

\renewcommand{\emph}{\textbf}

\title{Formulating Functional Formalisms for Fun}
\date{\thevenue \\ \thedate}
\author{Brent Yorgey}
% \titlegraphic{\includegraphics[width=1in]{tree}}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\begin{document}

\begin{xframe}{}
  \titlepage
  \note{I want to take you on a journey to the ancient past\dots}
\end{xframe}

\begin{xframe}{}
  \begin{center}
    {\Huge 1999}
  \end{center}
  \note{\dots the year 1999.  I was 17 years old. I had just learned
    Perl, and I thought regular expressions were the coolest thing
    since mini frosted wheat cereal. So I decided the obvious thing to
    do with my newfound string manipulation power was to implement a
    programming language.}
\end{xframe}

\begin{xframe}{}
  \begin{center}
    \lang{setlang}
  \end{center}
  \note{I don't remember why, but I decided to implement a language
    oriented around the evaluation of set expressions, including set
    literals and comprehensions.  The source code is lost, and I don't
    remember what I named it.  I didn't write it in a functional
    style, or in any style at all, really.  I didn't know about
    parsing or ASTs or type systems or compositional interpreters. And
    I definitely had no idea at the time that others had already done
    similar things.  But none of that mattered.  What I do remember is
    simply being obsessed with the idea of being able to create my own
    language, and the joy of being able to create not just a static
    artifact, but a whole new way of expressing myself.
  }
\end{xframe}

\begin{xframe}{}
  % \begin{center}
  %   \includegraphics[width=1in]{04bay.jpg}
  % \end{center}
  \note{I don't tell you this story to boast about how young I was,
    and I certainly don't mean to imply that you need to start
    programming as a teenager in order to be successful.  Obviously
    that's not true.  I'm very aware of the privilege it was for me to
  have the resources and opportunity to do that, and thankful for all
  the people and circumstances that made it possible.

  Rather, I tell you this story because it turned out to be the start
  of a lifelong obsession with building languages, and XXX.

  Many of the projects I'm most proud of, or that I learned the most
  from, or that brought me the most joy, are related in one way or
  another to building languages.  Maybe I'm just weird? We all
  are weird in our own unique ways, of course, but I don't think I'm
  weird in this.

  JOY of building languages.

  XXX also so you can see the languages that you may have heard of
  didn't form ex nihilo, I had lots of practice, toy throwaway things,
  etc. along the way
}
\end{xframe}

\begin{xframe}{}
  XXX Outline
  Going to tell you about some language-building projects I have been
  involved in,
\end{xframe}

\begin{xframe}{}
  Discord for questions!
\end{xframe}

\def\sectionimg{egypt}
\section{History}

\begin{xframe}{}
  \begin{center}
    \lang{Context Free Art} \bigskip

    \includegraphics[width=2in]{chalk.png} \smallskip

    {\scriptsize \emph{Chalk}, by chris, CC-BY 3.0} \\
    {\scriptsize \url{https://www.contextfreeart.org/gallery/view.php?id=256}}
  \end{center}
  \note{In maybe 2001 or thereabouts, I discovered Context Free Art, a
    language for generating images from context-free grammars.  It
    didn't have a feature I wanted (weighted random choice among
    multiple applicable rules), so I added it.
  }
\end{xframe}

\begin{xframe}{}
  \begin{center}
    \includegraphics[width=4in]{cfdg.png}
  \end{center}
  \note{This was actually my first open-source contribution\dots and
    23 years later, my name is still in a comment in the current
    source code!
  }
\end{xframe}

\begin{xframe}{}
  \begin{center}
    \lang{Photizon}
  \end{center}
  \note{In college, probably around 2002, I wrote a raytracer as a
    project for my graphics class, which I named Photizon.  The
    raytracer we were supposed to write supported only a very simple
    scene description language consisting essentially of a list of
    primitive shapes, attributes, and transformations.  But I wanted
    to render complex scenes like\dots}
\end{xframe}

\begin{xframe}{}
  \begin{center}
    \includegraphics[width=4in]{photizon_lego.png}
  \end{center}
  \note{\dots this, with thousands of primitive shapes.  So, of
    course, I made my own scene description language.  It had things
    for loops, conditionals, and function definitions; it was
    basically a macro system, and I wrote a preprocessor that expanded
    it into the simplified syntax.}
  \note{XXX again, created a language that let me express what I wanted!}
\end{xframe}

\begin{xframe}{}
  \begin{center}
    \lang{c2bef} \bigskip

    \includegraphics[width=5in]{befunge.png}
  \end{center}
  \note{Does anyone recognize this programming language?  This is
    Befunge, a truly demented esoteric programming language where the
    program counter is a 2D coordinate and a direction, and each
    character is an instruction.  I did NOT make this language.  But
    my best friend Steve Winslow and I, for our winter term project
    one year, made a compiler that translated a subset of C into
    Befunge.

    I don't know exactly why we decided to do this, but collaborating
    intensely for a month with my best friend was a lot of fun, and I
    learned a lot about parsing and compilers.
  }
\end{xframe}

\begin{xframe}{}
  \begin{center}
    \includegraphics[width=2in]{haskell.png}
  \end{center}
  \note{We're now up to about 2007, which is the year I fell in love
    with Haskell.  It's a fun story---feel free to ask me about it
    sometime---but since it's not the point of this talk I won't dwell
    on it.}
\end{xframe}

\begin{xframe}{}
  \begin{center}
    \lang{adventure} \bigskip

    \includegraphics[width=4in]{adventure.png}
  \end{center}
  \note{Apparently I must have read several papers about EDSLs,
    because one of the first things I did with Haskell, in 2008, was
    build a (clunky) EDSL for text adventure games.

    XXX what did I learn from this?
  }
\end{xframe}

\begin{xframe}{}
  \begin{center}
    \lang{species} \bigskip

    \includegraphics[width=3in]{species.png}
  \end{center}
  \note{Created a Haskell EDSL for calculating with combinatorial
    species. Looking back on it now, it's also pretty clunky.  But I
    built it mostly just to help me learn the theory of combinatorial
    species, and in that it was quite successful, since I ended up
    centering my PhD dissertation around them.}
\end{xframe}

\begin{xframe}{}
  \begin{center}
    \lang{GHC} \bigskip

    \includegraphics[width=1.5in]{cambridge.jpg} \note{I spent the
      summer of 2010 doing an internship at Microsoft Research in
      Cambridge.  (That's me punting on the Cam with my mother-in-law,
      my third-cousin-in-law, and Celeste the Travel Bear.)  Spent the
      summer thinking about promoting types to kinds, and helping
      implement an overhaul of GHC's constraint solver.  This one is
      different since it's not a language that I created, but I
      include it since XXX}
  \end{center}
\end{xframe}

\begin{xframe}{}
  \begin{center}
    \lang{diagrams} \bigskip

    \includegraphics[width=5in]{diagrams.png}
  \end{center}
    \note{2008: an EDSL for describing and rendering vector graphics.
      Many of you have probably heard of this library.  What you may
      not know is that the current version is the \emph{second}
      diagrams EDSL.  The one I made in 2008 was clunky and had some
      limiting design decisions.  After thinking about it for more
      than a year I rewrote the entire thing from scratch and released
      a much more elegant version in 2011.  Rewriting diagrams with a
      more elegant and formal semantic foundation was a formative
      experience for me.  I learned a lot about what Conal Elliot
      terms ``denotational design'', and I went through probably 15 or
      20 versions before finally converging on something that finally
      felt right.  It's not perfect, but I think it's stood the test
      of time pretty well.}
\end{xframe}

\begin{xframe}{}
  \begin{center}
    \includegraphics[width=2in]{diagrams-contributors.png}
  \end{center}
  \note{A significant aspect of this project was that this was my
    first open-source project to attract other contributors.  A few
    people became involved with diagrams 1 and then worked on the
    rewrite to diagrams 2, which attracted yet more contributors.  For
    quite a few years we had a vibrant virtual community of around
    5-10 people, some of whom remain my friends to this day, and there
  was a long tail of people who contributed in small ways.}
\end{xframe}

\def\sectionimg{eridanus_cropped}
\section{Building languages and being human}

\note{Let's take a step back.  Why build languages?  What is so joyful
  about it?}

\begin{xframe}
  \begin{center}
    \includegraphics[width=5in]{creation_of_adam}
  \end{center}
\end{xframe}

% Humans are many
% things, but according to my faith tradition, there are two primary
% things that characterize what it is to be human: we are made
% to be creative, and made to be in community.  (Image: Creation of
% Adam; [appropriate since it is in Italy, painted by Italian
% Michaelangelo =)]) And one of the primary ways we engage in both of
% those things is through the use of language---in imitation of God,
% who created the world by speaking.
%
% But you don't have to agree with my faith tradition---regardless of
% your beliefs, I think it's self-evident that creativity and
% relationship are at the core of what it is to be human.
%
% So building languages...
% - Creating.  Creating new means of creating!  Empowers others to
%   create.  Overflows into...
%
% - Community.
%
% - Learning.  Didn't mention curiosity/love of learning before, but
%   it's a pretty safe assumption that everyone listening to this talk
%   identifies with a desire and love for learning.  If you really
%   want to become a PL expert, there's no better way than building
%   languages---and it really makes no difference whether they are simple toy languages or
%   industrial-strength compilers or anything in between.
%
%   Bloom's taxonomy level 6.
%
% Of course, language can also be used to
% destroy and exclude...

\begin{xframe}{}
  \lang{Disco}
\end{xframe}

\begin{xframe}{}
  \lang{Swarm}
\end{xframe}

\begin{xframe}{}
  \lang{Let's build a language together!}
\end{xframe}

\end{document}
