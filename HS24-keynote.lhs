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

% \setbeameroption{show notes}

\renewcommand{\emph}{\textbf}

\title{Formulating Functional Formalisms for Fun}
\date{\thevenue \\ \thedate}
\author{Brent Yorgey}
% \titlegraphic{\includegraphics[width=1in]{tree}}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\begin{document}

\begin{xframe}{}
  \titlepage
  \note{So, I had to pick a title before I had fully decided what I
    was going to talk about, because Garrett was being a responsible
    chair and nagging me about it.  I thought the alliteration was
    cute, but I think it comes off perhaps as too frivolous.}
\end{xframe}

\title{\sout{Formulating Functional Formalisms for Fun} \\ The Joy of Building (Functional) Languages}

\begin{xframe}
  \titlepage \note{Perhaps a better title is this.  I'll let you
    decide as you listen to my talk.  In any case, I want to start by
    taking you on a journey to the ancient past\dots}
\end{xframe}

\begin{xframe}{}
  \begin{center}
    {\Huge 1999}
  \end{center}
  \note{\dots the year 1999.  I was 17 years old. I had just learned
    Perl, and I thought regular expressions were the coolest thing
    since frosted mini wheat cereal. So I decided the obvious thing to
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
    I definitely had no idea at the time that others had already made
    similar languages.  But none of that mattered.  What I do remember is
    simply being intoxicated with the idea of being able to create my own
    language, the joy of being able to create not just a static
    artifact, but a whole new way of expressing myself.
  }
\end{xframe}

\begin{xframe}{}
  % \begin{center}
  %   \includegraphics[width=1in]{04bay.jpg}
  % \end{center}
  \note{Why am I telling you this story?  I don't tell it to boast
    about how young I was, and I certainly don't mean to imply that
    you need to start programming as a teenager in order to be
    successful like me.  Obviously that's not true.  I'm very aware of
    the unusual privilege it was for me to have the resources and
    opportunity to do that, and thankful for all the people and
    circumstances that made it possible.}
\end{xframe}

\begin{xframe}{}

  \note{Rather, I tell you this story because it turned out to be the
    first in a long line of similar experiences.

    Many of the projects I'm most proud of, the ones that I learned
    the most from, that created the most community, that brought me
    the most joy---are related in one way or another to building
    languages. Maybe I'm just weird. We all are weird in our own
    unique ways, of course, but I don't think I'm weird in this.

    My goal today is to inspire you with the joy of building
    languages!  Maybe some of you work on a language implementation
    for your job: I hope to remind you of the joy in your work, or
    perhaps inspire you to build some toy languages on the side, just
    for fun.  And maybe some of you have never implemented your own
    language before, or maybe some of you have done so but not in a
    long time; I hope to inspire you as well.}
\end{xframe}

\begin{xframe}{}
  \begin{center}
    \includegraphics[width=2in]{discord-logo-blue}
  \end{center}
  \note{By the way, as a teacher I hate the idea of saving all the
    questions for the end, especially in a long presentation like
    this.  Feel free to interrupt me with questions; or you should
    also feel free to find the Discord channel for this talk
    (\texttt{ICFP Discord > Q\&A > haskell > Fabricating...}) and post
    questions or comments there.  I will have the channel open on my
    phone and I will respond!}
\end{xframe}

\def\sectionimg{egypt}
\section{More History}

\note{So let me tell you more of my personal history of building languages.}

\begin{xframe}{}
  \begin{center}
    \lang{Context Free Art} \bigskip

    \includegraphics[width=2in]{chalk.png} \smallskip

    {\scriptsize \emph{Chalk}, by chris, CC-BY 3.0} \\
    {\scriptsize \url{https://www.contextfreeart.org/gallery/view.php?id=256}}
  \end{center}
  \note{In 2001 or thereabouts, I discovered Context Free Art, a
    language for generating images from context-free grammars.  (By
    the way, it still exists, it's super fun, and you should try it!)
    It didn't have a feature I wanted, namely, weighted random choice
    among multiple rules, so I added it.  It wasn't a big
    change---maybe 50 lines of code max---but again, the feeling of being
    able to imagine a new way of expressing myself, and then bring it
    into being, was incredible.

    Incidentally, this was actually my first open-source
    contribution\dots}
\end{xframe}

\begin{xframe}{}
  \begin{center}
    \includegraphics[width=4in]{cfdg.png}
  \end{center}
  \note{\dots and 23 years later, my name is still in a comment in the
    source code!  How cool is that?  This is a screenshot from two
    days ago when I looked up the source on GitHub.}
\end{xframe}

\begin{xframe}{}
  \begin{center}
    \lang{Photizon} \bigskip

    \includegraphics[width=2in]{cyl3Mirrors}
  \end{center}
  \note{In college, probably around 2002, I wrote a raytracer as a
    project for my graphics class, which I named Photizon. (The name
    is Ancient Greek---ask me later if you enjoy nerding out about
    that kind of language.)  The raytracer we were supposed to write
    supported only a very simple scene description language consisting
    essentially of a list of primitive shapes, attributes, and
    transformations, like this.  But I wanted to render complex scenes like\dots}
\end{xframe}

\begin{xframe}{}
  \begin{center}
    \includegraphics[width=5in]{photizon_lego.png}
  \end{center}
  \note{\dots this, with thousands of primitive shapes.  So, of
    course, I made my own scene description language.  It supported
    things like imports, variables, for loops, and so on, and it
    essentially macro-expanded into the simplified syntax.

    I don't know if you can tell at all, but I had a lot of fun
    with that project\dots}
\end{xframe}

\begin{xframe}{}
  \begin{center}
    \lang{c2bef} \bigskip

    \includegraphics[width=5in]{befunge.png}
  \end{center}
  \note{Does anyone recognize this programming language?  This is
    Befunge, a truly demented esoteric programming language where the
    program counter is actually a triple of a row, a column, and a
    direction, and each character is an instruction.  I did NOT make
    this language.  But my best friend Steve Winslow and I, for our
    winter term project one year, made a compiler that translated a
    subset of C into Befunge.

    I don't remember exactly why we decided to do this, but collaborating
    intensely for a month with my best friend was a ton of fun, and I
    learned a lot about parsing and compilers.
  }
\end{xframe}

\begin{xframe}{}
  \begin{center}
    {\Huge 2007} \bigskip

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
  \note{Apparently, while learning Haskell, I must have read several
    papers about EDSLs, because one of the first nontrivial things I
    did with Haskell, in 2008, was to build a (rather clunky) EDSL for
    text adventure games.
  }
\end{xframe}

\begin{xframe}{}
  \begin{center}
    \lang{species} \bigskip

    \includegraphics[width=3in]{species.png}
  \end{center}
  \note{Then, in an entirely different vein, I created a Haskell EDSL
    for calculating with combinatorial species. (Don't worry if you
    don't know what that is.) Looking back on it now, it's also pretty
    clunky.  But I built it mostly just to help me learn the theory of
    combinatorial species, and in that it was spectacularly
    successful, since I ended up writing my entire PhD dissertation
    about them.}
\end{xframe}

\begin{xframe}{}
  \begin{center}
    \lang{GHC} \bigskip

    \includegraphics[width=1.5in]{cambridge.jpg} \note{I spent the
      summer of 2010 doing an internship at Microsoft Research in
      Cambridge.  (This is me punting on the Cam with my
      mother-in-law, my third-cousin-in-law, and Celeste the Travel
      Bear.)  I spent the summer thinking about things, probably about
      promoting types to kinds, as well as helping implement an
      overhaul of GHC's constraint solver.  This one is a bit
      different since it's not a language that I created, but I
      include it since I was still helping build a language.

      This was a wonderful experience---everyone, especially Simon
      Peyton Jones and Dimitris Vytiniotis, was kind and generous, and
      I learned a ton.  Being in Cambridge didn't hurt, either (as you
      can see).}
  \end{center}
\end{xframe}

\begin{xframe}{}
  \begin{center}
    \lang{diagrams} \bigskip

    \includegraphics[width=5in]{diagrams.png}
  \end{center}
  \note{In 2008, I made an EDSL for describing and rendering vector
    graphics.  Many of you have probably heard of this library.  What
    you may not know is that the version on Hackage now is the
    \emph{second} diagrams EDSL.  The one I made in 2008 was clunky
    and had some limiting design decisions, so I eventually threw it
    away and spent more than a year redesigning and reimplementing it
    from scratch, releasing the new, much more elegant version in
    2011.  Rewriting the diagrams EDSL with a more elegant and formal
    semantic foundation was a formative experience for me.  I learned
    a lot from Conal Elliot about ``denotational design'', and I went
    through probably 15 or 20 variants before finally converging on
    something that felt right.  It's still not perfect, but I think it's
    stood the test of time pretty well.}
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

\begin{xframe}{}
  \begin{center}
    \begin{minipage}{2.5in}
      \begin{center}
        \includegraphics[width=2.5in]{diagrams-strings} \\ {\tiny
          Celia Rubio-Madrigal, Jules Hedges} \medskip

        \includegraphics[width=2.5in]{kites-darts} \\ {\tiny Chris Reade}
      \end{center}
    \end{minipage}
    \begin{minipage}{2.5in}
      \begin{center}
        \includegraphics[width=2.5in]{parking-westminster.png} \\
        {\tiny Dominic Steinitz} \bigskip

        \includegraphics[width=2.5in]{server-I0.3.timed} \\ {\tiny Edsko de Vries, Duncan Coutts}
      \end{center}
    \end{minipage}
  \end{center}
  \note{The other really cool thing about the diagrams EDSL is to see
    all the ways other people have been empowered to make really cool
    stuff with it! What's on this slide is just a tiny sampling.}
\end{xframe}

\def\sectionimg{eridanus_cropped}
\section{Building languages and being human}

\note{Let's take a step back.  I hope I've convinced you that I, at
  least, find building languages to be joyful and meaningful.  But I
  don't think I'm at all the odd one out here.  Why do I say that?}

\begin{xframe}
  \begin{center}
    \includegraphics[width=5in]{creation_of_adam}
  \end{center}
  \note{Humans are many things, but according to my faith tradition,
    there are two primary things that characterize what it is to be
    human: we are made to be creative, and we are made to be in
    community.  And one of the primary ways we engage in both of those
    things is through the use of language---in imitation of God, who
    created the world by speaking.

    But you don't have to agree with my faith tradition---regardless of
    your beliefs, I think it's self-evident that creativity and
    relationship are somewhere at the core of what it is to be human.

    If we now think back over my experiences building languages,
    you'll notice three main themes:
  }
\end{xframe}

\begin{xframe}{}
  \begin{center}
    {\Huge Creativity}
  \end{center}
  \note{Building a language of course is a creative act, but it's more
    than that: it's creating a new means of being creative, and
    empowering others to create!  This naturally overflows into\dots
  }
\end{xframe}

\begin{xframe}{}
  \begin{center}
    {\Huge Community}
  \end{center}
  \note{Building a language often means building community.  I think
    we all know this on some level.  You'll notice this started
    happening more with my language projects as I gained experience
    and took on bigger projects.  Shared purpose, collaboration (which
    is just shared creativity) are wonderful for building community.}
\end{xframe}

\begin{xframe}{}
  \begin{center}
    {\Huge Learning}
  \end{center}
  \note{Last but not least, you'll notice I mentioned how much I
    \emph{learned} from many of my projects.  I hesitate to include
    ``love of learning'' on the same level as creativity and community
    in terms of what is fundamental to being human.  But I think it's
    a pretty safe assumption that everyone listening to this talk, at
    least, identifies with a desire and love for learning.  If you
    really want to become a PL expert, there's no better way than
    actually designing and implementing languages---and it really
    makes no difference whether they are simple toy languages or
    industrial-strength compilers or anything in between.}
\end{xframe}

\begin{xframe}{}
  \begin{center}
    \includegraphics[width=4in]{bloom}
  \end{center}
  \note{Maybe some of you who are teachers may recognize this.  This
    is called Bloom's Taxonomy, and it categorizes different levels of
    thinking and learning.  It's a helpful model both for planning and
    assessing the level at which teachers are requiring students to
    operate, but it's also helpful for anyone to reflect on their own
    learning.  You can see that being able to create reflects the
    highest level of learning---not only that, but engaging at each
    level also strengthens the levels below.  And you don't have to be
    an expert at each level before moving on to the next.  So trying
    to create is really the ultimate form of learning; it's no wonder
    that I learned so much from all of these projects.
  }
\end{xframe}

\begin{xframe}{}
  \begin{center}
    \input{diagrams/joy.pgf}
  \end{center}
  \note{This is why I keep using the word ``joy''.  To me it doesn't
    just mean ``happy'', though they can often go together.  Joy has
    to do with flourishing as a human being.  If all of these are
    happening, I would call that flourishing, I would call that
    joyful.  And not just happening to you individually, but that you
    are enabling and empowering others to experience this sort of
    flourishing as well.

    So I want to tell you about two more of my language projects, but
    for each one I will specifically highlight the ways that they led
    to creativity, community, and learning, for me and for others.}
\end{xframe}

\begin{xframe}{}
  \begin{center}
    \lang{Disco} \bigskip

    \includegraphics[width=1in]{disco}
  \end{center}
  \note{Disco began as a collaboration with Harley Eades at TFPIE
    2016.  We got excited about the idea of a language for teaching
    functional programming principles + discrete mathematics and spent
    a bunch of time developing some initial ideas, and since then it
    has grown and matured quite a lot, although it's still rough
    around the edges in some places.  I use it when I teach Discrete
    Math, typically to second-year undergraduates.  It is designed to
    be as close as possible to standard mathematical syntax and
    practice, to minimize the degree to which students feel they are
    being forced to learn two languages (the language of mathematics
    and a programming language).  It both exposes them to some
    functional programming ideas, and gives them a computational
    playground that helps them internalize the math concepts they are
    learning.}
\end{xframe}

\begin{xframe}{}
  \begin{center}
    \includegraphics[width=5in]{disco-code}
  \end{center}
  \note{Here's some example Disco code teaching students about set
    comprehensions (full circle!).  Required a lot of fun creativity
    to design a language to be close to mathematical syntax and
    practice, and easy for students to use. Led to some very
    non-traditional choices.  As a result, the type system is rather
    complex---and I had to learn a lot about how to make it work
    (subtyping + qualified polymorphism + equirecursive types, learned
    from Isabelle). As for community, there were some outside
    contributors early on, but many students have used it, and many
    have also gone on to contribute to its implementation.  Two are
    here with me this week, so Disco has been indirectly responsible
    for bringing new students into the FP community!}
\end{xframe}

\begin{xframe}{}
  \begin{center}
    \lang{Swarm} \bigskip

    \includegraphics[width=4in]{swarm-debugger}
  \end{center}
  \note{Swarm is a 2D resource gathering and programming game.  The
    only way you can interact with the world is by programming robots
    to go out and explore and collect resources for you; collecting
    resources allows you to craft devices that unlock additional
    programming language features.  Something I started during my
    sabbatical fall 2021.  So far I've learned a lot about CESK
    machines, effects type systems, how to deal with variable frame
    rates, and a bunch of other things.  Has attracted a small but
    thriving developer community.  I hope one day it will also attract
    a community of player-developers who will not only play the game
    but construct and contribute new levels/scenarios.}
\end{xframe}

\begin{xframe}{}
  \begin{center}
    \url{https://swarm-game.github.io/}
  \end{center}
  \note{If you want to try it out, please do, it's a lot of fun!
    Still some big missing features but we would love to hear your
    feedback.}
\end{xframe}

\begin{xframe}{}
  \begin{center}
    \lang{Let's build a language together!} \bigskip

    \url{https://github.com/byorgey/HS24-keynote}
  \end{center}

  \note{I have talked \emph{about} building languages enough, let's
    actually build one together!  Too risky/long to just completely
    take your suggestions, so I have something in mind already. But I'd
    love your suggestions via Discord and even your help with pull
    requests via GitHub (just be judicious---rather not deal with
    merge conflicts).  Explain idea of language etc.}
\end{xframe}

\end{document}
