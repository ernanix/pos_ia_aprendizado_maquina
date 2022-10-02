import random
OPTIONS = [(-1, 0), (0, 1), (1, 0), (0, -1), (0, 0)]

class Cell:
    def __init__(self, r):
        self.r = r
        self.q_list = [0, 0, 0, 0, 0]


matrix = [[Cell(10), Cell(0), Cell(6)],
          [Cell(0), Cell(-10), Cell(-10)],
          [Cell(0), Cell(0), Cell(4)]]

n = 1
pos = [0, 0]

f = open("output.txt", "w")
f.write("")
f.close()
f = open("output.txt", "a")
f.write("Q Learning Test\n")

for ii in range(3):
    for jj in range(3):
        pos[0] = ii
        pos[1] = jj
        for step in range(99):
            cell = matrix[pos[0]][pos[1]]
            for i in range(len(cell.q_list)):
                ng_pos = [pos[0] + OPTIONS[i][0], pos[1] + OPTIONS[i][1]]
                if ng_pos[0] > 2 or ng_pos[0] < 0:
                    continue
                if ng_pos[1] > 2 or ng_pos[1] < 0:
                    continue

                ng = matrix[ng_pos[0]][ng_pos[1]]
                cell.q_list[i] = cell.q_list[i] + 0.8 * (ng.r + 0.9 * max(ng.q_list) - cell.q_list[i])

            # print q list
            q_list_int = [int(q) for q in cell.q_list]
            print(f"{n}.\t{pos[0] + 1}, {pos[1] + 1}: \t{q_list_int}")
            f.write(f"{n}.\t{pos[0] + 1}, {pos[1] + 1}: \t{q_list_int}\n")
            n += 1

            # change to random neighbor cell
            while True:
                x = pos[0]
                y = pos[1]
                pos_change = random.choice(OPTIONS)

                x += pos_change[0]
                if x > 2 or x < 0:
                    continue
                else:
                    pos[0] = x
                y += pos_change[1]
                if y > 2 or y < 0:
                    continue
                else:
                    pos[1] = y
                break

        # check cells by highest
        for step in range(11):
            cell = matrix[pos[0]][pos[1]]
            for i in range(len(cell.q_list)):
                ng_pos = [pos[0] + OPTIONS[i][0], pos[1] + OPTIONS[i][1]]
                if ng_pos[0] > 2 or ng_pos[0] < 0:
                    continue
                if ng_pos[1] > 2 or ng_pos[1] < 0:
                    continue

                ng = matrix[ng_pos[0]][ng_pos[1]]
                cell.q_list[i] = cell.q_list[i] + 0.8 * (ng.r + 0.9 * max(ng.q_list) - cell.q_list[i])

            # print q list
            q_list_int = [int(q) for q in cell.q_list]
            print(f"{n}.\t{pos[0] + 1}, {pos[1] + 1}: \t{q_list_int}")
            f.write(f"{n}.\t{pos[0] + 1}, {pos[1] + 1}: \t{q_list_int}\n")
            n += 1

            # change to neighbor cell with the highest max q
            temp_q_list = cell.q_list
            temp_options = OPTIONS
            while True:
                x = pos[0]
                y = pos[1]
                pos_change = temp_options[temp_q_list.index(max(temp_q_list))]

                x += pos_change[0]
                if x > 2 or x < 0:
                    temp_options.pop(temp_q_list.index(max(temp_q_list)))
                    temp_q_list.remove(max(temp_q_list))
                    continue
                pos[0] = x

                y += pos_change[1]
                if y > 2 or y < 0:
                    temp_options.pop(temp_q_list.index(max(temp_q_list)))
                    temp_q_list.remove(max(temp_q_list))
                    continue
                pos[1] = y

                break
print("Cima  Dir  Baixo  Esq  Centro")

print("\nTabela final de Qs máximos:")
print(f"   \t1,1\t1,2\t1,3\t2,1\t2,2\t2,3\t3,1\t3,2\t3,3")
print(f"1,1\t"
      f"{int(matrix[0][0].q_list[4])}\t{int(matrix[0][0].q_list[1])}\t0\t"
      f"{int(matrix[0][0].q_list[2])}\t0\t0\t"
      f"0\t0\t0")
print(f"1,2\t"
      f"{int(matrix[0][1].q_list[3])}\t{int(matrix[0][1].q_list[4])}\t{int(matrix[0][1].q_list[1])}\t"
      f"0\t{int(matrix[0][1].q_list[2])}\t0\t"
      f"0\t0\t0")
print(f"1,3\t"
      f"0\t{int(matrix[0][2].q_list[3])}\t{int(matrix[0][2].q_list[4])}\t"
      f"0\t0\t{int(matrix[0][2].q_list[2])}\t"
      f"0\t0\t0")
print(f"2,1\t"
      f"{int(matrix[1][0].q_list[0])}\t0\t0\t"
      f"{int(matrix[1][0].q_list[4])}\t{int(matrix[1][0].q_list[1])}\t0\t"
      f"{int(matrix[1][0].q_list[2])}\t0\t0")
print(f"2,2\t"
      f"0\t{int(matrix[1][1].q_list[0])}\t0\t"
      f"{int(matrix[1][1].q_list[3])}\t{int(matrix[1][1].q_list[4])}\t{int(matrix[1][1].q_list[1])}\t"
      f"0\t{int(matrix[1][1].q_list[2])}\t0")
print(f"2,3\t"
      f"0\t0\t{int(matrix[1][2].q_list[0])}\t"
      f"0\t{int(matrix[1][2].q_list[3])}\t{int(matrix[1][2].q_list[4])}\t"
      f"0\t0\t{int(matrix[1][2].q_list[2])}")
print(f"3,1\t"
      f"0\t0\t0\t"
      f"{int(matrix[2][0].q_list[0])}\t0\t0\t"
      f"{int(matrix[2][0].q_list[4])}\t{int(matrix[2][0].q_list[1])}\t0")
print(f"3,2\t"
      f"0 \t0\t0\t"
      f"0\t{int(matrix[2][1].q_list[0])}\t0\t"
      f"{int(matrix[2][1].q_list[3])}\t{int(matrix[2][1].q_list[4])}\t{int(matrix[2][1].q_list[1])}")
print(f"3,3\t"
      f"0\t0\t0\t"
      f"0\t0\t{int(matrix[2][2].q_list[0])}\t"
      f"0\t{int(matrix[2][2].q_list[3])}\t{int(matrix[2][2].q_list[4])}")


f.write("Cima  Dir  Baixo  Esq  Centro\n")

f.write("\nTabela final de Qs máximos:\n")
f.write(f"   \t1,1\t1,2\t1,3\t2,1\t2,2\t2,3\t3,1\t3,2\t3,3\n")
f.write(f"1,1\t"
      f"{int(matrix[0][0].q_list[4])}\t{int(matrix[0][0].q_list[1])}\t0\t"
      f"{int(matrix[0][0].q_list[2])}\t0\t0\t"
      f"0\t0\t0\n")
f.write(f"1,2\t"
      f"{int(matrix[0][1].q_list[3])}\t{int(matrix[0][1].q_list[4])}\t{int(matrix[0][1].q_list[1])}\t"
      f"0\t{int(matrix[0][1].q_list[2])}\t0\t"
      f"0\t0\t0\n")
f.write(f"1,3\t"
      f"0\t{int(matrix[0][2].q_list[3])}\t{int(matrix[0][2].q_list[4])}\t"
      f"0\t0\t{int(matrix[0][2].q_list[2])}\t"
      f"0\t0\t0\n")
f.write(f"2,1\t"
      f"{int(matrix[1][0].q_list[0])}\t0\t0\t"
      f"{int(matrix[1][0].q_list[4])}\t{int(matrix[1][0].q_list[1])}\t0\t"
      f"{int(matrix[1][0].q_list[2])}\t0\t0\n")
f.write(f"2,2\t"
      f"0\t{int(matrix[1][1].q_list[0])}\t0\t"
      f"{int(matrix[1][1].q_list[3])}\t{int(matrix[1][1].q_list[4])}\t{int(matrix[1][1].q_list[1])}\t"
      f"0\t{int(matrix[1][1].q_list[2])}\t0\n")
f.write(f"2,3\t"
      f"0\t0\t{int(matrix[1][2].q_list[0])}\t"
      f"0\t{int(matrix[1][2].q_list[3])}\t{int(matrix[1][2].q_list[4])}\t"
      f"0\t0\t{int(matrix[1][2].q_list[2])}\n")
f.write(f"3,1\t"
      f"0\t0\t0\t"
      f"{int(matrix[2][0].q_list[0])}\t0\t0\t"
      f"{int(matrix[2][0].q_list[4])}\t{int(matrix[2][0].q_list[1])}\t0\n")
f.write(f"3,2\t"
      f"0 \t0\t0\t"
      f"0\t{int(matrix[2][1].q_list[0])}\t0\t"
      f"{int(matrix[2][1].q_list[3])}\t{int(matrix[2][1].q_list[4])}\t{int(matrix[2][1].q_list[1])}\n")
f.write(f"3,3\t"
      f"0\t0\t0\t"
      f"0\t0\t{int(matrix[2][2].q_list[0])}\t"
      f"0\t{int(matrix[2][2].q_list[3])}\t{int(matrix[2][2].q_list[4])}\n")
