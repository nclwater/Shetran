#
# Comparison plotting routines
#

# General Imports
import os

# Standard Packages
from matplotlib import pyplot as plt
import numpy as np
import pandas as pd


def plot_col_differences(df_combined: pd.DataFrame, col: str, fn_figure: str,
                         metrics: dict) -> None:
    """
    Generates and saves a plot comparing 'should' and 'is' timeseries data,
    along with their absolute and percentage differences.

    Args:
        df_combined (pd.DataFrame): DataFrame with 'should', 'is', 'diff_abs', 'diff_pct' columns.
        col (str): The name of the data column being plotted.
        fn_figure (str): The output filename for the plot image.
        metrics (dict): A dictionary of similarity metrics to display.
    """
    plt.style.use("dark_background")
    fig = plt.figure(figsize=(20, 12), constrained_layout=True)
    fig.suptitle(f"Comparison for column: {col}", fontsize=16)

    # Use GridSpec for a more complex layout: 3 rows, 2 columns.
    # The first column is for the legend/text, the second for the plots.
    gs = fig.add_gridspec(3, 2, width_ratios=[1, 5])

    # --- Subplots for data ---
    ax1 = fig.add_subplot(gs[0, 1])
    ax2 = fig.add_subplot(gs[1, 1], sharex=ax1)
    ax3 = fig.add_subplot(gs[2, 1], sharex=ax1)

    # --- Subplot for legend and text ---
    ax_legend = fig.add_subplot(gs[:, 0])

    # --- Top plot: Timeseries ---
    (line1, ) = ax1.plot(df_combined.index,
                         df_combined["should"],
                         label="Expected",
                         color="cyan")
    (line2, ) = ax1.plot(
        df_combined.index,
        df_combined["is"],
        label="Actual",
        color="magenta",
        linestyle="--",
    )
    ax1.set_title("Timeseries")
    ax1.grid(True, linestyle=":", alpha=0.5)
    plt.setp(ax1.get_xticklabels(),
             visible=False)  # Hide x-axis labels for top and middle plots

    # --- Middle plot: Absolute Difference ---
    ax2.plot(df_combined.index, df_combined["diff_abs"], color="red")
    ax2.set_title("Absolute Difference")
    ax2.grid(True, linestyle=":", alpha=0.5)
    plt.setp(ax2.get_xticklabels(), visible=False)

    # --- Bottom plot: Percentage Difference ---
    ax3.plot(df_combined.index,
             df_combined["diff_pct"].fillna(np.nan),
             color="orange")
    ax3.set_title("Percentage Difference")
    ax3.set_xlabel("Time")
    ax3.grid(True, linestyle=":", alpha=0.5)

    # --- Legend and Metrics Box ---
    ax_legend.axis("off")
    # Place legend in the upper part of the legend axis
    ax_legend.legend(handles=[line1, line2],
                     loc="center left",
                     bbox_to_anchor=(0, 0.85))

    # Format metrics text and place it in a box below the legend
    metrics_text = "Similarity Metrics:\n"

    def _format_metric_value(value) -> str:
        try:
            numeric_value = float(value)
            if np.isfinite(numeric_value):
                return f"{numeric_value:.4f}"
            return "nan"
        except (TypeError, ValueError):
            return str(value)

    metrics_text += "\n".join(
        [f"  {k}: {_format_metric_value(v)}" for k, v in metrics.items()])
    ax_legend.text(
        0.0,
        0.7,
        metrics_text,
        transform=ax_legend.transAxes,
        fontsize=12,
        verticalalignment="top",
        bbox=dict(boxstyle="round,pad=0.5", fc="black", ec="grey", lw=1),
    )

    # Save and close the figure to free up memory
    os.makedirs(os.path.dirname(fn_figure), exist_ok=True)
    plt.savefig(fn_figure)
    plt.close(fig)

    return None


def plot_hdf5_map(diff_map: np.ndarray,
                  title: str,
                  fn_figure: str,
                  cmap: str = "viridis") -> None:
    """Plot a 2D difference map and save it to disk."""
    plt.style.use("dark_background")
    fig, ax = plt.subplots(figsize=(10, 8), constrained_layout=True)

    img = ax.imshow(diff_map, aspect="auto", origin="lower", cmap=cmap)
    ax.set_title(title)
    ax.set_xlabel("x")
    ax.set_ylabel("y")
    cbar = fig.colorbar(img, ax=ax)
    cbar.set_label("difference")

    os.makedirs(os.path.dirname(fn_figure), exist_ok=True)
    plt.savefig(fn_figure)
    plt.close(fig)

    return None
