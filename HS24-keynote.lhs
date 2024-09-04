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
    {\Large 1. setlang}
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
  \begin{center}
    \includegraphics[width=1in]{04bay.jpg}
  \end{center}
  \note{I don't tell you this story to boast about how young I was,
    and I certainly don't mean to imply that you need to start
    programming as a teenager in order to be successful.  Obviously
    that's not true.  I'm very aware of the privilege it was for me to
  have the resources and opportunity to do that, and thankful for all
  the people and circumstances that made it possible.

  Rather, I tell you this story because it turned out to be the start
  of a lifelong obsession with building languages, and XXX.

  JOY of building languages.
}
\end{xframe}

\begin{xframe}{}
  \begin{center}
    {\Large 2. Photizon}
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
    course, I made my own scene description language.}
\end{xframe}

% Why build languages?  What is so joyful about it?  Humans are many
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


\end{document}
