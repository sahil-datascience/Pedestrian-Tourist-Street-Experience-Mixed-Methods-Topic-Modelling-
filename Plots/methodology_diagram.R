# Install if needed:
# install.packages(c("DiagrammeR", "DiagrammeRsvg", "rsvg"))

library(DiagrammeR)
# Optional for export:
library(DiagrammeRsvg); library(rsvg)

# DOT diagram: methodological workflow

dot <- '
digraph G {
  graph [rankdir=TB, fontsize=12, splines=true, nodesep=0.8, ranksep=1.0, bgcolor="transparent"]
  node [shape=rectangle, style="rounded,filled", fontsize=12, fontname="Times-Roman",
        fontcolor="#111111", color="#2b2b2b", margin="0.35,0.22", penwidth=1.2]
  edge [color="#6c6c6c", arrowsize=0.9, fontname="Times-Roman", fontsize=10, penwidth=1.0]

  /* ---------- Top cluster: Data Collection & Cleaning ---------- */
  subgraph cluster_data {
    label = "Data Collection & Cleaning";
    labelloc = "t";
    color="#cfe8ff"; style="rounded,filled"; fillcolor="#fbfeff";
    node [fillcolor="#eef8ff"]
    A_data [label=<
      <TABLE BORDER="0" CELLBORDER="0" CELLSPACING="0">
        <TR><TD><B>1. Data collection</B></TD></TR>
        <TR><TD>(TripAdvisor; Mall Road &amp; M.G. Road)</TD></TR>
      </TABLE>
    >]
    B_clean [label=<
      <TABLE BORDER="0" CELLBORDER="0" CELLSPACING="0">
        <TR><TD><B>2. Cleaning &amp; Import</B></TD></TR>
        <TR><TD>deduplication; merge taglines</TD></TR>
      </TABLE>
    >]
  }

  /* ---------- Middle row: Feature Engineering (3 cols) ---------- */
  subgraph cluster_features {
    label = "Feature Engineering";
    labelloc = "t";
    color="#fff0d9"; style="rounded,filled"; fillcolor="#fffdf7";
    node [fillcolor="#fff6e8"]
    C_text [label=<
      <TABLE BORDER="0" CELLBORDER="0" CELLSPACING="0">
        <TR><TD><B>3a. Text features</B></TD></TR>
        <TR><TD>tokenize + POS filter</TD></TR>
      </TABLE>
    >]
    C_spat [label=<
      <TABLE BORDER="0" CELLBORDER="0" CELLSPACING="0">
        <TR><TD><B>3b. Spatial features</B></TD></TR>
        <TR><TD>city/country; remove locals</TD></TR>
      </TABLE>
    >]
    C_temp [label=<
      <TABLE BORDER="0" CELLBORDER="0" CELLSPACING="0">
        <TR><TD><B>3c. Temporal features</B></TD></TR>
        <TR><TD>season: Winter/Spring/Summer/Autumn</TD></TR>
      </TABLE>
    >]
  }

  /* ---------- DFM centered ---------- */
  F_dfm [label=<
    <TABLE BORDER="0" CELLBORDER="0" CELLSPACING="0">
      <TR><TD><B>4. Document-Feature Matrix (DFM)</B></TD></TR>
      <TR><TD>Shimla: 3652×3235 | Gangtok: 1360×1692</TD></TR>
    </TABLE>
  >, fillcolor="#e6fbff", color="#2b7fa6", penwidth=1.4]

  /* ---------- Left column: Topic Modelling & Qualitative ---------- */
  subgraph cluster_topic {
    label = "Topic Modelling & Qualitative";
    labelloc = "t";
    color="#e8ddff"; style="rounded,filled"; fillcolor="#fbf8ff";
    node [fillcolor="#efe8ff"]
    G_stm [label=<
      <TABLE BORDER="0" CELLBORDER="0" CELLSPACING="0">
        <TR><TD><B>5. STM Topic Modelling</B></TD></TR>
        <TR><TD>K selection: coherence / exclusivity</TD></TR>
      </TABLE>
    >]
    H_qual [label=<
      <TABLE BORDER="0" CELLBORDER="0" CELLSPACING="0">
        <TR><TD><B>6. Qualitative Exploration</B></TD></TR>
        <TR><TD>color-coded HTML, memos, coding</TD></TR>
      </TABLE>
    >]
  }

  /* ---------- Right column: Sentiment Analysis (separate cluster) ---------- */
  subgraph cluster_sent {
    label = "Sentiment Analysis";
    labelloc = "t";
    color="#ffdfe6"; style="rounded,filled"; fillcolor="#fffafb";
    node [fillcolor="#ffecec"]
    I_gold [label=<
      <TABLE BORDER="0" CELLBORDER="0" CELLSPACING="0">
        <TR><TD><B>7a. Gold standard</B></TD></TR>
        <TR><TD>n = 478; stratified</TD></TR>
      </TABLE>
    >]
    J_llm [label=<
      <TABLE BORDER="0" CELLBORDER="0" CELLSPACING="0">
        <TR><TD><B>7b. LLM vs Lexicon</B></TD></TR>
        <TR><TD>LLM evaluation &amp; labelling (gpt-oss:20B)</TD></TR>
      </TABLE>
    >]
  }

  /* ---------- Bottom cluster: Integration & Focus ---------- */
  subgraph cluster_integ {
    label = "Integration & Analysis";
    labelloc = "t";
    color="#dff9ea"; style="rounded,filled"; fillcolor="#fbfffb";
    node [fillcolor="#e8fff0"]
    K_link [label=<
      <TABLE BORDER="0" CELLBORDER="0" CELLSPACING="0">
        <TR><TD><B>8. Integration</B></TD></TR>
        <TR><TD>Link topics &lt;-&gt; sentiment; seasonal &amp; origin analysis</TD></TR>
      </TABLE>
    >]
    L_focus [label=<
      <TABLE BORDER="0" CELLBORDER="0" CELLSPACING="0">
        <TR><TD><B>9. Focus on negative subset</B></TD></TR>
        <TR><TD>Interpretation &amp; implications</TD></TR>
      </TABLE>
    >]
  }

  /* ---------- Rank/order hints to get left/right arrangement ----------

     Important: nodes are placed left-to-right on a rank in the order listed.
     To place sentiment on the right and topic on the left, include them
     together with the topic nodes in the same rank but ordered:
     I_gold (left spacer) ; G_stm ; H_qual ; J_llm (right spacer)
  ---------- */

  { rank = same; A_data; }         /* top center */
  { rank = same; B_clean; }        /* below data */
  { rank = same; C_text; C_spat; C_temp; }  /* three features in a row */
  { rank = same; F_dfm; }          /* centered */
  { rank = same; I_gold; G_stm; H_qual; J_llm; } /* left topics, right sentiment */
  { rank = same; K_link; L_focus; } /* bottom centered */

  /* ---------- Edges (main flow) ---------- */
  A_data -> B_clean

  B_clean -> C_text
  B_clean -> C_spat
  B_clean -> C_temp

  C_text -> F_dfm


  F_dfm -> G_stm
  H_qual -> G_stm 
  G_stm -> K_link
  H_qual -> K_link

  /* Sentiment nodes feed into integration (right side visually) */
  I_gold -> K_link
  J_llm -> K_link

  /* Dashed contextual feeds from temporal/spatial into integration */
  C_temp -> K_link [style="dashed", color="#2b8f57", fontcolor="#2b8f57", label="seasonal"]
  C_spat -> K_link [style="dashed", color="#2b8f57", fontcolor="#2b8f57", label="spatial"]

  K_link -> L_focus

  /* ---------- A couple of invisible edges to nudge spacing (optional) ---------- */
  /* If layout needs slight horizontal push, uncomment next lines:
  I_gold -> G_stm [style="invis"];
  J_llm -> H_qual [style="invis"];
  */
}

'



dot <- '
digraph G {
  /* ========== Global / publication-friendly settings ========== */
  graph [
    rankdir = TB,
    fontsize = 11,
    splines = true,
    nodesep = 0.6,
    ranksep = 0.9,
    bgcolor = "transparent",
    ratio = "fill"
  ]

  node [
    shape = rectangle,
    style = "rounded,filled",
    fontsize = 11,
    fontname = "Times-Roman",
    fontcolor = "#111111",
    color = "#444444",
    margin = "0.30,0.18",
    penwidth = 0.8
  ]

  edge [
    color = "#666666",
    arrowsize = 0.8,
    fontname = "Times-Roman",
    fontsize = 9,
    penwidth = 0.7
  ]

  /* ======== Section background: subtle pale frames ========
     Each section is an outer cluster with a very pale fill to act as a background.
     Inner clusters / nodes sit inside. This gives clear meta-areas for the 4 sections.
  ========================================================== */

  /* ---------- Section 1: Data Preprocessing & Feature Engineering ---------- */
  subgraph cluster_section_preproc {
    label = "Data Preprocessing & Feature Engineering";
    labelloc = "t";
    color = "#e6eef6";        /* pale border tint */
    style = "rounded,filled";
    fillcolor = "#fbfdff";
    penwidth = 0.4;

    /* Data collection & cleaning */
    subgraph cluster_preproc {
      label = "Data collection & cleaning";
      labelloc = "t";
      color = "#d7e6f8";
      style = "rounded,filled";
      fillcolor = "#f2f8ff";
      node [fillcolor = "#f7fbff"]

      A_data [label=<
        <TABLE BORDER="0" CELLBORDER="0" CELLSPACING="0">
          <TR><TD><B>1. Data collection</B></TD></TR>
          <TR><TD>(TripAdvisor; Mall Road &amp; M.G. Road)</TD></TR>
        </TABLE>
      >]

      B_clean [label=<
        <TABLE BORDER="0" CELLBORDER="0" CELLSPACING="0">
          <TR><TD><B>2. Cleaning &amp; import</B></TD></TR>
          <TR><TD>deduplication; merge taglines</TD></TR>
        </TABLE>
      >]
    }

    /* Feature engineering row */
    subgraph cluster_features {
      label = "Feature engineering";
      labelloc = "t";
      color = "#f7efe0";
      style = "rounded,filled";
      fillcolor = "#fffdf8";
      node [fillcolor = "#fffaf3"]

      C_text [label=<
        <TABLE BORDER="0" CELLBORDER="0" CELLSPACING="0">
          <TR><TD><B>3a. Text features</B></TD></TR>
          <TR><TD>tokenize; POS filter</TD></TR>
        </TABLE>
      >]

      C_spat [label=<
        <TABLE BORDER="0" CELLBORDER="0" CELLSPACING="0">
          <TR><TD><B>3b. Spatial features</B></TD></TR>
          <TR><TD>city/country; remove locals</TD></TR>
        </TABLE>
      >]

      C_temp [label=<
        <TABLE BORDER="0" CELLBORDER="0" CELLSPACING="0">
          <TR><TD><B>3c. Temporal features</B></TD></TR>
          <TR><TD>season: Winter / Spring / Summer / Autumn</TD></TR>
        </TABLE>
      >]
    }

    /* DFM node (still in preproc section) */
    F_dfm [label=<
      <TABLE BORDER="0" CELLBORDER="0" CELLSPACING="0">
        <TR><TD><B>4. Document–Feature Matrix</B></TD></TR>
        <TR><TD>Shimla: 3652×3235  |  Gangtok: 1360×1692</TD></TR>
      </TABLE>
    >, fillcolor="#eefcff", color="#2b7fa6", penwidth=0.9]
  }

  /* ---------- Section 2: Topic Modelling (left) ---------- */
  subgraph cluster_section_topic {
    label = "Topic Modelling";
    labelloc = "t";
    color = "#efe8ff";
    style = "rounded,filled";
    fillcolor = "#fbf8ff";
    penwidth = 0.4;

    subgraph cluster_topic {
      label = "";
      node [fillcolor = "#f5f0ff"]

      G_stm [label=<
        <TABLE BORDER="0" CELLBORDER="0" CELLSPACING="0">
          <TR><TD><B>5. STM topic modelling</B></TD></TR>
          <TR><TD>K selection: coherence / exclusivity</TD></TR>
        </TABLE>
      >]

      H_qual [label=<
        <TABLE BORDER="0" CELLBORDER="0" CELLSPACING="0">
          <TR><TD><B>6. Qualitative exploration</B></TD></TR>
          <TR><TD>color-coded HTML, memos, coding</TD></TR>
        </TABLE>
      >]
    }
  }

  /* ---------- Section 3: Sentiment Analysis (right) ---------- */
  subgraph cluster_section_sent {
    label = "Sentiment Analysis";
    labelloc = "t";
    color = "#fde6eb";
    style = "rounded,filled";
    fillcolor = "#fff7f9";
    penwidth = 0.4;

    subgraph cluster_sent {
      label = "";
      node [fillcolor = "#fff0f2"]

      I_gold [label=<
        <TABLE BORDER="0" CELLBORDER="0" CELLSPACING="0">
          <TR><TD><B>7a. Gold standard</B></TD></TR>
          <TR><TD>n = 478; stratified sampling</TD></TR>
        </TABLE>
      >]

      J_llm [label=<
        <TABLE BORDER="0" CELLBORDER="0" CELLSPACING="0">
          <TR><TD><B>7b. LLM vs lexicon</B></TD></TR>
          <TR><TD>LLM evaluation &amp; labelling (gpt-oss:20B)</TD></TR>
        </TABLE>
      >]
    }
  }

  /* ---------- Section 4: Mixed Methods (bottom) ---------- */
  subgraph cluster_section_mixed {
    label = "Mixed Methods";
    labelloc = "t";
    color = "#e8f7ee";
    style = "rounded,filled";
    fillcolor = "#fbfffb";
    penwidth = 0.4;

    subgraph cluster_integ {
      label = "";
      node [fillcolor = "#f0fff4"]

      K_link [label=<
        <TABLE BORDER="0" CELLBORDER="0" CELLSPACING="0">
          <TR><TD><B>8. Integration</B></TD></TR>
          <TR><TD>Link topics &lt;-&gt; sentiment; seasonal &amp; origin analysis</TD></TR>
        </TABLE>
      >]

      L_focus [label=<
        <TABLE BORDER="0" CELLBORDER="0" CELLSPACING="0">
          <TR><TD><B>9. Focus on negative subset</B></TD></TR>
          <TR><TD>Interpretation &amp; implications</TD></TR>
        </TABLE>
      >]
    }
  }

  /* ========== Rank ordering to control left-right placement ========== */
  { rank = same; A_data; }
  { rank = same; B_clean; }
  { rank = same; C_text; C_spat; C_temp; }
  { rank = same; F_dfm; }
  /* Place I_gold (left spacer), topic nodes, then J_llm (right spacer) */
  { rank = same; I_gold; G_stm; H_qual; J_llm; }
  { rank = same; K_link; L_focus; }

  /* ========== Edges: flow ========== */
  A_data -> B_clean

  B_clean -> C_text
  B_clean -> C_spat
  B_clean -> C_temp

  C_text -> F_dfm
  C_spat -> F_dfm
  C_temp -> F_dfm

  F_dfm -> G_stm
  G_stm -> H_qual
  H_qual -> K_link
  G_stm -> K_link

  /* Sentiment -> Integration */
  I_gold -> K_link
  J_llm -> K_link

  /* Contextual dashed links */
  C_temp -> K_link [style="dashed", color="#2b8f57", fontcolor="#2b8f57", label="seasonal"]
  C_spat -> K_link [style="dashed", color="#2b8f57", fontcolor="#2b8f57", label="spatial"]

  K_link -> L_focus

  /* Slight invisible nudges (comment/uncomment as needed):
     Use invis edges if you need to push sentiment further away horizontally.
  */
  /* I_gold -> G_stm [style="invis"]; */
  /* J_llm -> H_qual [style="invis"]; */
}

'

monochrome_dot <- '
digraph G {
  /* ========== Global / publication-ready monochrome settings ========== */
  graph [
    rankdir = TB,
    fontsize = 10,
    splines = true,
    nodesep = 0.6,
    ranksep = 0.9,
    bgcolor = "transparent",
    ratio = "fill"
  ]

  /* Default node style: fixed width/height for consistent visual columns */
  node [
    shape = rectangle,
    style = "rounded,filled",
    fontsize = 10,
    fontname = "Times-Roman",
    fontcolor = "#111111",
    color = "#666666",
    fillcolor = "#ffffff",
    margin = "0.25,0.16",
    penwidth = 0.6,
    width = 2.6,      /* consistent width (in inches) */
    height = 0.7,     /* consistent height; increase if labels clip */
    fixedsize = true  /* enforce same width/height across nodes */
  ]

  edge [
    color = "#666666",
    arrowsize = 0.8,
    fontname = "Times-Roman",
    fontsize = 9,
    penwidth = 0.6
  ]

  /* ---------- Section background frames (very subtle gray borders, white fill) ---------- */
  subgraph cluster_section_preproc {
    label = "Data Preprocessing & Feature Engineering";
    labelloc = "t";
    color = "#dddddd";
    style = "rounded,filled";
    fillcolor = "#ffffff";  /* white background for printing */
    penwidth = 0.4;

    /* Data collection & cleaning */
    subgraph cluster_preproc {
      label = "";
      node [fillcolor="#ffffff"]

      A_data [label=<
        <TABLE BORDER="0" CELLBORDER="0" CELLSPACING="0">
          <TR><TD><B>1. Data collection</B></TD></TR>
          <TR><TD>(TripAdvisor; Mall Road &amp; M.G. Road)</TD></TR>
        </TABLE>
      >]

      B_clean [label=<
        <TABLE BORDER="0" CELLBORDER="0" CELLSPACING="0">
          <TR><TD><B>2. Cleaning &amp; import</B></TD></TR>
          <TR><TD>deduplication; merge taglines</TD></TR>
        </TABLE>
      >]
    }

    /* Feature engineering row */
    subgraph cluster_features {
      label = "";
      node [fillcolor="#ffffff"]

      C_text [label=<
        <TABLE BORDER="0" CELLBORDER="0" CELLSPACING="0">
          <TR><TD><B>3a. Text features</B></TD></TR>
          <TR><TD>tokenize; POS filter</TD></TR>
        </TABLE>
      >]

      C_spat [label=<
        <TABLE BORDER="0" CELLBORDER="0" CELLSPACING="0">
          <TR><TD><B>3b. Spatial features</B></TD></TR>
          <TR><TD>city/country; remove locals</TD></TR>
        </TABLE>
      >]

      C_temp [label=<
        <TABLE BORDER="0" CELLBORDER="0" CELLSPACING="0">
          <TR><TD><B>3c. Temporal features</B></TD></TR>
          <TR><TD>season: Winter / Spring / Summer / Autumn</TD></TR>
        </TABLE>
      >]
    }

    /* DFM node included in preprocessing area */
    F_dfm [label=<
      <TABLE BORDER="0" CELLBORDER="0" CELLSPACING="0">
        <TR><TD><B>4. Document–Feature Matrix (DFM)</B></TD></TR>
        <TR><TD>Shimla: 3652×3235  |  Gangtok: 1360×1692</TD></TR>
      </TABLE>
    >]
  }

  /* ---------- Topic modelling (left) ---------- */
  subgraph cluster_section_topic {
    label = "Topic Modelling";
    labelloc = "t";
    color = "#dddddd";
    style = "rounded,filled";
    fillcolor = "#ffffff";
    penwidth = 0.4;

    G_stm [label=<
      <TABLE BORDER="0" CELLBORDER="0" CELLSPACING="0">
        <TR><TD><B>5. STM topic modelling</B></TD></TR>
        <TR><TD>K selection: coherence / exclusivity</TD></TR>
      </TABLE>
    >]

    H_qual [label=<
      <TABLE BORDER="0" CELLBORDER="0" CELLSPACING="0">
        <TR><TD><B>6. Qualitative exploration</B></TD></TR>
        <TR><TD>color-coded HTML, memos, coding</TD></TR>
      </TABLE>
    >]
  }

  /* ---------- Sentiment analysis (right) ---------- */
  subgraph cluster_section_sent {
    label = "Sentiment Analysis";
    labelloc = "t";
    color = "#dddddd";
    style = "rounded,filled";
    fillcolor = "#ffffff";
    penwidth = 0.4;

    I_gold [label=<
      <TABLE BORDER="0" CELLBORDER="0" CELLSPACING="0">
        <TR><TD><B>7a. Gold standard</B></TD></TR>
        <TR><TD>n = 478; stratified sampling</TD></TR>
      </TABLE>
    >]

    J_llm [label=<
      <TABLE BORDER="0" CELLBORDER="0" CELLSPACING="0">
        <TR><TD><B>7b. LLM vs lexicon</B></TD></TR>
        <TR><TD>LLM evaluation &amp; labelling (gpt-oss:20B)</TD></TR>
      </TABLE>
    >]
  }

  /* ---------- Mixed methods (bottom) ---------- */
  subgraph cluster_section_mixed {
    label = "Mixed Methods";
    labelloc = "t";
    color = "#dddddd";
    style = "rounded,filled";
    fillcolor = "#ffffff";
    penwidth = 0.4;

    K_link [label=<
      <TABLE BORDER="0" CELLBORDER="0" CELLSPACING="0">
        <TR><TD><B>8. Integration</B></TD></TR>
        <TR><TD>Link topics &lt;-&gt; sentiment; seasonal &amp; origin analysis</TD></TR>
      </TABLE>
    >]

    L_focus [label=<
      <TABLE BORDER="0" CELLBORDER="0" CELLSPACING="0">
        <TR><TD><B>9. Focus on negative subset</B></TD></TR>
        <TR><TD>Interpretation &amp; implications</TD></TR>
      </TABLE>
    >]
  }

  /* ========== Rank ordering to control left-right placement ========== */
  { rank = same; A_data; }
  { rank = same; B_clean; }
  { rank = same; C_text; C_spat; C_temp; }
  { rank = same; F_dfm; }
  { rank = same; I_gold; G_stm; H_qual; J_llm; }
  { rank = same; K_link; L_focus; }

  /* ========== Edges (flow) ========== */
  A_data -> B_clean

  B_clean -> C_text
  B_clean -> C_spat
  B_clean -> C_temp

  C_text -> F_dfm
  C_spat -> F_dfm
  C_temp -> F_dfm

  F_dfm -> G_stm
  G_stm -> H_qual
  H_qual -> K_link
  G_stm -> K_link

  I_gold -> K_link
  J_llm -> K_link

  /* contextual dashed links */
  C_temp -> K_link [style="dashed", color="#666666", fontcolor="#666666", label="seasonal"]
  C_spat -> K_link [style="dashed", color="#666666", fontcolor="#666666", label="spatial"]

  K_link -> L_focus

  /* Optional invisible nudges (uncomment to push sentiment wider): */
  /* I_gold -> G_stm [style="invis"]; */
  /* J_llm -> H_qual [style="invis"]; */
}


'

        
# Render the graph
grViz(dot)
grViz(monochrome_dot)

# --- Export (optional) ---
svg_xml <- DiagrammeRsvg::export_svg(grViz(dot))
charToRaw(svg_xml) %>% rsvg::rsvg_png("Plots/methodology_workflow.png")
rsvg::rsvg_svg(charToRaw(svg_xml), "Plots/methodology_workflow.svg")

#----------------------------------------------------------------
#
#
#---------------------------------------------------------------

