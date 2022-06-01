from random import randint, random, uniform
import matplotlib.pyplot
import numpy as np
import pandas as pd
from pygad import GA
import os

# def f(xn:list):
#     if()


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

# a = list(raw_material_stock["raw materials"])
# b = list(product_material.columns)[1:]
# print(a == b)
# a = list(product_profit["product"])
# b = list(product_material["product"])
# print(a == b)
# a = list(product_profit["product"])
# b = list(product_material["product"])
# c = list(minimal_production["product"])
# print(a == b == c)


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


def fitness_f(solution, solution_idx):
    """
    Ada di Jurnal pada halaman 5 "fitness function",

    Where c1 – c6 are constraints referring to
    equation 2 and c7 is a constraint in accordance
    with equation (3).
    """
    solution = np.abs(solution)
    assert len(PRODUCT_PROFIT) == len(solution)
    c1 = constraint_material(
        solution, PRODUCT_MATERIAL["fabric"], RAW_MATERIAL["stock"][0]
    )
    c2 = constraint_material(
        solution, PRODUCT_MATERIAL["yarn"], RAW_MATERIAL["stock"][1]
    )
    c3 = constraint_material(
        solution, PRODUCT_MATERIAL["rubber"], RAW_MATERIAL["stock"][2]
    )
    c4 = constraint_material(
        solution, PRODUCT_MATERIAL["zipper"], RAW_MATERIAL["stock"][3]
    )
    c5 = constraint_material(
        solution, PRODUCT_MATERIAL["rope"], RAW_MATERIAL["stock"][4]
    )
    c6 = constraint_material(
        solution, PRODUCT_MATERIAL["silicon"], RAW_MATERIAL["stock"][5]
    )
    c7 = constraint_minimum(solution)
    return f(solution) - 2000 * (c1 + c2 + c3 + c4 + c5 + c6 + c7)


def clear_output_folder():
    """Menghapus output folder beserta konten-kontennya"""
    current_directory, _ = os.path.split(os.path.abspath(__file__))
    image_directory = os.path.join(current_directory, "output")
    if os.path.exists(image_directory):
        files_names = os.listdir(image_directory)
        for f in files_names:
            os.remove(os.path.join(image_directory, f))


def do_ga(
    population_size=8,
    crossover_probability=0.35,
    mutation_probability=0.10,
):
    """Membuat fungsi untuk menjalankan ga dengan beberapa parameter yang bisa diubah, jika tidak diubah maka akan menggunakan nilai default-nya
    Parameter yang bisa diubah menyesuaikan pada soal yaitu:
    1. population_size (default 4)
    2. crossover_probability (default 0.25)
    3. mutation_probability (default 0.1)
    """
    # Mendefinisikan jumlah generasi total sebelum program berhenti (total iterasi)
    num_generations = 5000
    # Mendefinisikan jumlah solusi yang akan dianggap sebagai parent
    num_parents_mating = 2

    # Mendefinisikan variabel fitness function agar bisa dijadikan parameter
    fitness_function = fitness_f

    # Mendefinisikan panjang kromosom, yang mana sama dengan jumlah function input
    num_genes = len(PRODUCT_PROFIT)

    # Mendefinisikan batas bawah dan batas atas angka random pada kromosom pertama
    init_range_low = 0
    init_range_high = 20

    # Memilih parent selection tipe tournament.
    # Kami memilih metode ini dikarenakan tipe tournament karena dapat memproses nilai fitness yang negative,
    # sedangkan metode roulete wheel tidak bisa.
    parent_selection_type = "tournament"

    # Memilih tipe crossover sesuai single point (memiliki satu garis batas) dikarenakan
    # memiliki nilai fitness paling stabil menaik
    crossover_type = "single_point"

    # Memilih tipe mutation inversion, dikarenakan menghasilkan hasil yang paling baik. Jika mutasi
    # adalah random, maka akan menghasilkan gen yang negatif, tidak cocok untuk kasus kita yang memiliki
    # constraint positive.
    mutation_type = "inversion"

    # Melakukan definisi algoritma genetic algorithm dengan library PYGAD
    # memasukan parameter sesuai dengan yang sudah didefinisikan diatas
    ga_instance = GA(
        gene_type=np.int64,
        num_generations=num_generations,
        num_parents_mating=num_parents_mating,
        fitness_func=fitness_function,
        sol_per_pop=population_size,
        num_genes=num_genes,
        init_range_low=init_range_low,
        init_range_high=init_range_high,
        parent_selection_type=parent_selection_type,
        crossover_type=crossover_type,
        crossover_probability=crossover_probability,
        mutation_type=mutation_type,
        mutation_probability=1 - mutation_probability,
    )

    # Menjalankan algoritma
    ga_instance.run()

    # Menampilkan solusi terbaik setelah algoritma selesai dijalankan
    solution, solution_fitness, solution_idx = ga_instance.best_solution()
    # Menyimpan output pada file
    filename = f"{solution_fitness}_output"
    current_directory = os.path.dirname(os.path.realpath(__file__))
    output_directory = os.path.join(current_directory, "output")
    output_file = os.path.join(output_directory, filename + ".txt")
    if not os.path.exists(output_directory):
        os.makedirs(output_directory)

    with open(output_file, "w") as file:
        file.write(
            f"""
population_size={population_size}
crossover_probability={crossover_probability}
mutation_probability={mutation_probability}"""
        )
        file.write(f"Kromosom dari solusi terbaik adalah {solution}\n")
        file.write(
            f"Fitness value dari kromosom solusi terbaik adalah {solution_fitness}\n"
        )
        file.write(
            f"solusi terbaik tersebut ditemukan pada kromosom ke-{solution_idx} dari generasi ke-{ga_instance.best_solution_generation}\n"
        )

    # Melakukan plotting dari fitness setiap generasi dan menyimpannya ke komputer
    output_image = os.path.join(output_directory, filename) + ".png"
    matplotlib.pyplot.figure()
    matplotlib.pyplot.plot(ga_instance.best_solutions_fitness)
    matplotlib.pyplot.savefig(output_image)
    matplotlib.pyplot.close()
    # ga_instance.plot_fitness(save_dir=output_image)


def test():
    s = "18  2 18  2 12  2 18  2  2  2 18 18 18 12 18 18  2  2 18 18"
    s = s.split(" ")
    p = []
    for x in s:
        try:
            p.append(int(x))
        except:
            pass
    a = np.array(p)
    for x, i in enumerate(PRODUCT_MATERIAL.keys()):
        if i == 0:
            continue
    b = PRODUCT_MATERIAL.loc[:, "fabric":].transpose() * a
    print(np.sum(b, axis=1))
    print(RAW_MATERIAL)


if __name__ == "__main__":
    # for i in range(10000):
    #     pop_size = randint(6, 10)
    #     cp = uniform(0, 0.5)
    #     mp = uniform(0, 0.15)
    #     do_ga(
    #         population_size=pop_size, crossover_probability=cp, mutation_probability=mp
    #     )
    test()
