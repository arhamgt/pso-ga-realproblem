import numpy as np
import pandas as pd
import os


# Data bahan mentah setiap bulannya
RAW_MATERIAL = pd.read_csv("Table 1.csv")
print("Raw material and it stock")
print(RAW_MATERIAL)

# Data harga setiap produk
PRODUCT_PROFIT = pd.read_csv("Table 2.csv")
print("Profit per product")
print(PRODUCT_PROFIT)

# Data bahan mentah yang dibutuhkan setiap produk
PRODUCT_MATERIAL = pd.read_csv("Table 3.csv")
print("Minimum material per product")
print(PRODUCT_MATERIAL)


PRODUCT_MINIMAL = pd.DataFrame(
    [
        {"product": "pillowcases normal", "minimum": 0},
        {"product": "pillowcases seat", "minimum": 0},
        {"product": "pillowcases love", "minimum": 0},
        {"product": "pillowcases relaxing", "minimum": 0},
        {"product": "pillowcases baby", "minimum": 0},
        {"product": "bolsters baby", "minimum": 0},
        {"product": "bolsters adult", "minimum": 0},
        {"product": "bed linen 180", "minimum": 0},
        {"product": "bed linen 160", "minimum": 0},
        {"product": "bed linen 150", "minimum": 0},
        {"product": "bed linen 120", "minimum": 0},
        {"product": "bed linen 100", "minimum": 0},
        {"product": "bed linen 90", "minimum": 0},
        {"product": "pillow normal", "minimum": 0},
        {"product": "pillow seat", "minimum": 0},
        {"product": "pillow love", "minimum": 0},
        {"product": "pillow relaxing", "minimum": 0},
        {"product": "pillow baby", "minimum": 0},
        {"product": "bolster baby", "minimum": 0},
        {"product": "bolster adult", "minimum": 0},
    ]
)
print("Minimum production per product")
print(PRODUCT_MINIMAL)


def f(xn):
    """Di jurnal fungsi 1 halaman 4"""
    assert len(PRODUCT_PROFIT) == len(xn)

    production = np.sum(xn)
    sum = 0
    if production <= 600:
        for i, x in enumerate(xn):
            sum += PRODUCT_PROFIT["price"][i] * x
        return sum
    else:
        for i, x in enumerate(xn):
            sum += PRODUCT_PROFIT["price"][i] * x
        minus = 500 * (production - 600)
        return sum - minus


def constraint_material(xn, material_per_product, max_material):
    """Di jurnal berupa fungsi 2 halaman 4"""
    sum = 0
    for x, material in zip(xn, material_per_product):
        sum += x * material
    if sum <= max_material:
        return 0
    else:
        return sum - max_material


def constraint_minimum(xn):
    """Di jurnal fungsi 3 halaman 4"""
    sum = 0
    for i, x in enumerate(xn):
        if x >= PRODUCT_MINIMAL["minimum"][i]:
            sum += 0
        else:
            sum += x - PRODUCT_MINIMAL["minimum"][i]
    return sum


def fitness(xn):
    """
    Ada di Jurnal pada halaman 5 "fitness function",

    Where c1 – c6 are constraints referring to
    equation 2 and c7 is a constraint in accordance
    with equation (3).
    """
    assert len(PRODUCT_PROFIT) == len(xn)
    c1 = constraint_material(xn, PRODUCT_MATERIAL["fabric"], RAW_MATERIAL["stock"][0])
    c2 = constraint_material(xn, PRODUCT_MATERIAL["yarn"], RAW_MATERIAL["stock"][1])
    c3 = constraint_material(xn, PRODUCT_MATERIAL["rubber"], RAW_MATERIAL["stock"][2])
    c4 = constraint_material(xn, PRODUCT_MATERIAL["zipper"], RAW_MATERIAL["stock"][3])
    c5 = constraint_material(xn, PRODUCT_MATERIAL["rope"], RAW_MATERIAL["stock"][4])
    c6 = constraint_material(xn, PRODUCT_MATERIAL["silicon"], RAW_MATERIAL["stock"][5])
    c7 = constraint_minimum(xn)
    return f(xn) - 2000 * (c1 + c2 + c3 + c4 + c5 + c6 + c7)


def clear_output_folder():
    """Menghapus output folder beserta konten-kontennya"""
    current_directory, _ = os.path.split(os.path.abspath(__file__))
    image_directory = os.path.join(current_directory, "output")
    if os.path.exists(image_directory):
        files_names = os.listdir(image_directory)
        for f in files_names:
            os.remove(os.path.join(image_directory, f))


def do_pso(
    image_name="default",
):
    # Inisiasi nilai
    n = len(PRODUCT_PROFIT)
    c1 = c2 = 1  # konstanta
    v = [0, 0, 0, 0, 0, 0]  # kecepatan awal
    x = [-2, 9, 4, 2, -1, -7]  # posisi awal

    r1 = [0, 0.1, 0.1, 0.3, 0.5]
    r2 = [0, 0.2, 0.4, 0.7, 1]
    omega = [0, 0.33, 0.48, 0.21, 0.99]

    t = 5  #jumlah iterasi
    for i in range(t):

        # reset nilai fitness untuk tiap iterasi
        fitnes = 0
        # itung nilai fitness tiap particle
        for j in range(n):
            # kalo iterasi 1x langsung, assign nilai pbest ama gbest
            if(i == 0):
                fitnes = fitness
                if(j == 0):
                    gbest_value = fitnes
                    gbest_pos = j
                elif(fitnes < pbest):
                    gbest_value = fitnes
                    gbest_pos = j

                pbest = gbest_value

                continue
            # dalam code ini, fitness sudah menjasi nilai minimum fitness seluruh partikel
            pbest = fitnes

            # update velocity & posisi
            v[j] = omega[i] * v[j-1] + c1 * r1[i] * (pbest - x[i-1]) + c2 * r2[i] * (gbest_value - x[i-1])
            x[j] = v[j] + x[j-1]

            # kalo nilai fungsi lebih kecil dari fitness, assign nilai fungsi pada fitness
            if(fitnes > fitness(x[j])):
                fitnes = fitness(x[j])

            # kalo nilai fitness lebih kecil dari gbest, assign nilai fitness pada gbest
            if(gbest_value > fitnes):
                gbest_value = fitnes
                gbest_pos = x[j]

    # Menjalankan algoritma


    # Menampilkan solusi terbaik setelah algoritma selesai dijalankan
    solution_fitness, solution_idx = gbest_value, gbest_pos
    print(f"Value terbaik {solution_fitness} di posisi {solution_idx}")
    # Menyimpan output pada file
    # current_directory = os.path.dirname(os.path.realpath(__file__))
    # output_directory = os.path.join(current_directory, "output")
    # output_file = os.path.join(output_directory, image_name + "output.txt")
    # if not os.path.exists(output_directory):
    #     os.makedirs(output_directory)
    #
    # with open(output_file, "w") as file:
    #     #file.write(f"Kromosom dari solusi terbaik adalah {solution}\n")
    #     file.write(
    #         f"Fitness value dari kromosom solusi terbaik adalah {solution_fitness}\n"
    #     )
    #     file.write(
    #         f"solusi terbaik tersebut ditemukan pada kromosom ke-{solution_idx}"
    #     )

    # Melakukan plotting dari fitness setiap generasi dan menyimpannya ke komputer
    # output_image = os.path.join(output_directory, image_name + " plot")
    # ga_instance.plot_fitness(save_dir=output_image)

if __name__ == "__main__":
    do_pso()

