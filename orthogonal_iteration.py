#!/bin/python3

import numpy as np
import matplotlib.pyplot as plt

np.random.seed()

# ==================================================
# DEFINE MATRICES AND SELECT ONE AT RANDOM
# ==================================================

matrices = {

    'Tridiagonal positive':
    np.array([
        [ 2, -1,  0],
        [-1,  2, -1],
        [ 0, -1,  2]
    ]),

    'All positive':
    np.array([
        [ 2,  1,  1],
        [ 1,  2,  1],
        [ 1,  1,  2]
    ]),

    'Tridiagonal positive off-diagonal':
    np.array([
        [ 2,  1,  0],
        [ 1,  2,  1],
        [ 0,  1,  2]
    ]),

    'Tridiagonal ones':
    np.array([
        [ 1,  1,  0],
        [ 1,  1,  1],
        [ 0,  1,  1]
    ]),

    'Zero diagonal':
    np.array([
        [ 0,  1,  0],
        [ 1,  0,  1],
        [ 0,  1,  0]
    ]),
    
    'Asymmetric':
    np.array([
        [ 1,  1,  1],
        [ 1,  1,  1],
        [ 0,  1,  1]
    ]),

    'Random symmetric':
    (
        lambda B: (B + B.T) / 2
    )(np.random.randn(3, 3)),

    'Destroys a subspace':
    np.array([
        [ 1,  -1,  0],
        [ -1,  1,  0],
        [ 0,  0,  1]
    ]),


    'Constructed':
    # np.array([[281, 120,   0],
    #    [120, 164,   0],
    #    [  0,   0,   1]]),

    np.array([
        [ 8,  5,  0],
        [ 5,  -8,  0],
        [ 0,  0,  1]])@ np.array([
        [ 4,  0,  0],
        [ 0,  1,  0],
        [ 0,  0,  1]
    ])@ (np.array([
        [ 8,  5,  0],
        [ 5,  -8,  0],
        [ 0,  0,  1]]).T),
    
    'Fibonacci':
    np.array([
        [ 1,  1,  0],
        [ 1,  0,  0],
        [ 0,  0,  1]
    ]),
    
   'Non-Normal':
    np.array([
        [ 1,  10,  0],
        [ 0,  2,  0],
        [ 0,  0,  1]
    ]),
}


# Randomly select a matrix
matrix_name = np.random.choice(list(matrices.keys()))
# or to just pick the final matrix
# matrix_name = list(matrices.keys())[-1]

A = matrices[matrix_name]

print("\nSelected matrix:", matrix_name)


# ==================================================
# CALCULATE THEORETICAL EIGENVALUES/VECTORS
# ==================================================

eigenvalues, eigenvectors = np.linalg.eig(A)

print("\nTheoretical eigenvalues:")
print(eigenvalues)

print("\nTheoretical eigenvectors:")
print(eigenvectors)


# ==================================================
# DEFINE STARTING VECTORS
# ==================================================

initial_vectors = {
    'Vector 1': np.array([1., 1., 1.]),
    'Vector 2': np.array([1., -1., 0.]),
    'Vector 3': np.array([1., 1., -2.])
}

# Number of original vectors to retain
n_keep = 0

# Random seed for reproducibility
np.random.seed(42)

# Keep the first n_keep vectors
initial_vectors = dict(
    list(initial_vectors.items())[:n_keep]
)

# Generate random replacements
n_random_vectors = 3 - n_keep

for i in range(n_random_vectors):
    initial_vectors[f'Random {i+1}'] = np.random.randn(3)


print("Matrix A:")
print(A)

print("\nInitial vectors:")

starting_vectors = {
    name: v.copy()
    for name, v in initial_vectors.items()
}

for name, v in starting_vectors.items():
    print(f"{name}: {v}")


# ==================================================
# ORTHOGONALISATION FUNCTION
# ==================================================

def orthogonalise(V):
    """Orthogonalise columns of V without normalising them."""

    Q = np.zeros_like(V, dtype=float)

    for i in range(V.shape[1]):

        v = V[:, i].copy()

        for j in range(i):
            v -= (
                np.dot(Q[:, j], v)
                / np.dot(Q[:, j], Q[:, j])
            ) * Q[:, j]

        Q[:, i] = v

    return Q


# ==================================================
# ORTHOGONAL ITERATION
# ==================================================

n_iterations = 10

names = list(initial_vectors.keys())

histories = {
    name: [v.copy()]
    for name, v in initial_vectors.items()
}

for iteration in range(n_iterations):

    # Multiply each vector by A
    V = np.column_stack([
        A @ initial_vectors[name]
        for name in names
    ])

    # Orthogonalise without normalising
    Q = orthogonalise(V)

    # Update vectors and store history
    for i, name in enumerate(names):

        initial_vectors[name] = Q[:, i].copy()

        histories[name].append(
            Q[:, i].copy()
        )


# Convert histories to arrays
for name in names:
    histories[name] = np.array(histories[name])


print("\nFinal vectors:")

for name, v in initial_vectors.items():
    print(f"{name}: {v}")


# ==================================================
# CREATE FIGURE LAYOUT
# ==================================================

fig = plt.figure(figsize=(18, 10))

gs = fig.add_gridspec(
    2, 6,
    height_ratios=[1, 1.2],
    width_ratios=[1, 1, 1, 1, 1, 1]
)


# ==================================================
# THREE NORMALISED VECTOR COMPONENTS (TOP ROW)
# ==================================================

axes = [
    fig.add_subplot(gs[0, 0:2]),
    fig.add_subplot(gs[0, 2:4]),
    fig.add_subplot(gs[0, 4:6])
]


# Fade settings (relative to initial magnitude)

fade_start = 1e-2
magnitude_cutoff = 1e-14


def vector_opacity(magnitude, initial_magnitude):
    """Calculate opacity from relative vector magnitude."""

    relative_magnitude = magnitude / initial_magnitude

    if relative_magnitude <= magnitude_cutoff:
        return 0.0

    if relative_magnitude >= fade_start:
        return 1.0

    # Logarithmic fade
    alpha = (
        np.log10(relative_magnitude)
        - np.log10(magnitude_cutoff)
    ) / (
        np.log10(fade_start)
        - np.log10(magnitude_cutoff)
    )

    return alpha


# Plot normalised components with magnitude-dependent fading

for i, (name, history) in enumerate(histories.items()):

    color = f'C{i}'

    magnitudes = np.linalg.norm(
        history,
        axis=1
    )

    initial_magnitude = magnitudes[0]

    # Calculate normalised components only where reliable
    normalised = np.full_like(history, np.nan)

    for iteration in range(len(history)):

        if magnitudes[iteration] > (
            magnitude_cutoff * initial_magnitude
        ):

            normalised[iteration] = (
                history[iteration] / magnitudes[iteration]
            )

    # Add legend entry
    for j in range(3):
        axes[j].plot(
            [],
            [],
            'o-',
            color=color,
            label=name
        )

    # Plot individual components
    for j in range(3):

        for iteration in range(len(history)):

            alpha = vector_opacity(
                magnitudes[iteration],
                initial_magnitude
            )

            if alpha == 0:
                continue

            # Plot component marker
            axes[j].plot(
                iteration,
                normalised[iteration, j],
                'o',
                color=color,
                alpha=alpha
            )

            # Connect to previous point if reliable
            if iteration > 0:

                previous_alpha = vector_opacity(
                    magnitudes[iteration - 1],
                    initial_magnitude
                )

                if previous_alpha > 0:

                    axes[j].plot(
                        [iteration - 1, iteration],
                        [
                            normalised[iteration - 1, j],
                            normalised[iteration, j]
                        ],
                        color=color,
                        alpha=min(alpha, previous_alpha),
                        linewidth=1.5
                    )


# Add theoretical eigenvector reference lines

for j, ax in enumerate(axes):

    for k in range(3):

        limit = eigenvectors[j, k]

        ax.axhline(
            limit,
            color=f'C{k+1}',
            linestyle='--',
            alpha=0.8,
            label=f'Eigenvector {k+1}'
        )

        ax.axhline(
            -limit,
            color=f'C{k+1}',
            linestyle='--',
            alpha=0.8
        )

    ax.set_title(f'Component {j+1}')

    ax.set_xlabel('Iteration')
    ax.set_ylabel('Normalised value')

    ax.set_ylim(-1, 1)

    ax.grid(True)
    ax.legend(fontsize=7)


# ==================================================
# MATRIX A (BOTTOM LEFT)
# ==================================================

ax_matrix = fig.add_subplot(gs[1, 0:2])

im = ax_matrix.imshow(
    A,
    cmap='viridis'
)

fig.colorbar(
    im,
    ax=ax_matrix,
    label='Matrix value'
)

for i in range(A.shape[0]):

    for j in range(A.shape[1]):

        ax_matrix.text(
            j,
            i,
            f'{A[i, j]:.0f}',
            ha='center',
            va='center',
            color='white',
            fontsize=14
        )

ax_matrix.set_title('Matrix A')


# ==================================================
# GROWTH OF VECTOR MAGNITUDES (BOTTOM MIDDLE)
# ==================================================

ax_mag = fig.add_subplot(gs[1, 2:4])

for name, history in histories.items():

    magnitudes = np.linalg.norm(
        history,
        axis=1
    )

    ax_mag.plot(
        magnitudes,
        'o-',
        label=name
    )


# Add theoretical exponential growth lines

iterations = np.arange(n_iterations + 1)

for k, eigenvalue in enumerate(eigenvalues):

    # Theoretical growth from unit initial magnitude
    theoretical_growth = np.abs(eigenvalue ** iterations)

    ax_mag.plot(
        iterations,
        theoretical_growth,
        color=f'C{k+1}',
        linestyle='--',
        alpha=0.8,
        label=f'Eigenvalue {k+1} = {eigenvalue:.3f}'
    )


ax_mag.set_xlabel('Iteration')
ax_mag.set_ylabel('Vector magnitude')

ax_mag.set_title('Growth of Trial Vectors')

ax_mag.set_yscale('log')

ax_mag.legend(fontsize=8)
ax_mag.grid(True)


# ==================================================
# 3D PLOT: THEORETICAL EIGENVECTORS AND ITERATED VECTORS
# ==================================================

ax_eig = fig.add_subplot(gs[1, 4:6], projection="3d")

# Plot theoretical eigenvector directions as black arrows.
# Eigenvectors are normalised here so they can be compared with
# the normalised directions of the iterated trial vectors.
plotted_eigenvectors = 0

for k, eigenvalue in enumerate(eigenvalues):
    eigenvector = eigenvectors[:, k]

    # Ordinary real 3D axes cannot display genuinely complex directions.
    if not np.allclose(eigenvector.imag, 0, atol=1e-10):
        print(
            f"Skipping eigenvector {k + 1}: it is genuinely complex "
            "and cannot be represented as a real 3D direction."
        )
        continue

    direction = np.real(eigenvector)
    norm = np.linalg.norm(direction)
    if norm == 0:
        continue
    direction = direction / norm

    ax_eig.quiver(
        0, 0, 0,
        direction[0], direction[1], direction[2],
        color="black",
        linewidth=2,
        arrow_length_ratio=0.12,
        label=f"Eigenvector {k + 1}, λ = {eigenvalue.real:.3f}"
    )
    ax_eig.quiver(
        0, 0, 0,
        -direction[0], -direction[1], -direction[2],
        color="black",
        linewidth=1,
        alpha=0.25,
        arrow_length_ratio=0.12
    )
    plotted_eigenvectors += 1


# Plot each trial vector's direction at every iteration.
# Normalising for display keeps the trajectory visible even when the
# actual vector magnitudes grow or shrink dramatically.
for i, (name, history) in enumerate(histories.items()):
    color = f"C{i}"

    magnitudes = np.linalg.norm(history, axis=1)
    valid = magnitudes > 1e-14

    directions = np.full_like(history, np.nan, dtype=float)
    directions[valid] = history[valid] / magnitudes[valid, np.newaxis]

    valid_directions = directions[valid]

    if len(valid_directions) == 0:
        continue

    # Connect successive directions to show the path through iterations.
    ax_eig.plot(
        valid_directions[:, 0],
        valid_directions[:, 1],
        valid_directions[:, 2],
        color=color,
        marker="o",
        markersize=3.5,
        linewidth=1.5,
        label=f"{name} (iterations)"
    )

    # Highlight the starting and final directions.
    first = valid_directions[0]
    last = valid_directions[-1]

    ax_eig.scatter(
        first[0], first[1], first[2],
        color=color,
        marker="s",
        s=35,
        edgecolors="black",
        linewidths=0.5
    )
    ax_eig.scatter(
        last[0], last[1], last[2],
        color=color,
        marker="*",
        s=85,
        edgecolors="black",
        linewidths=0.5
    )

# Unit-sphere limits make the direction comparison geometrically fair.
ax_eig.set_xlim(-1, 1)
ax_eig.set_ylim(-1, 1)
ax_eig.set_zlim(-1, 1)
ax_eig.set_box_aspect((1, 1, 1))
ax_eig.set_xlabel("x")
ax_eig.set_ylabel("y")
ax_eig.set_zlabel("z")
ax_eig.set_title("Eigenvectors and iterated vector directions")
ax_eig.view_init(elev=22, azim=35)

if plotted_eigenvectors:
    ax_eig.legend(fontsize=6, loc="upper left")
else:
    ax_eig.text2D(
        0.1, 0.5,
        "No real eigenvectors to display.",
        transform=ax_eig.transAxes
    )


# ==================================================
# FINAL LAYOUT
# ==================================================

eigenvalue_text = ', '.join(
    f'{value:.3f}' for value in eigenvalues
)

fig.suptitle(
    f'Orthogonal Iteration and Vector Evolution '
    f': {matrix_name} '
    f'(Eigenvalues: {eigenvalue_text})',
    fontsize=14
)

#fig.tight_layout(rect=[0, 0, 1, 0.92])
fig.tight_layout()

plt.show()
