
#E:\LSEPS\CateCode\Non-para的副本\result\500

# grf non-censor data

import xlwt
import os
import pandas as pd


def get_file_name(file_dir):
    for root, dirs, files in os.walk(file_dir):
        # print('root ', root) #当前目录路径
        # print('dirs ', dirs) #当前路径下所有子目录
        # print('files ', files) #当前路径下所有非目录子文件
        # pass

        return files

# get the dict {gama: rmse_list}, where rmse_list includes the rmse of the 4 group in the current game group
index_dict = {}
for index in range(18):
    if index % 3 == 0:
        index_dict[index] = 'logit'
    elif index % 3 == 1:
        index_dict[index] = 'CBPS1'
    else:
        index_dict[index] = 'CBPS2'
normal_dict = {}
mis_dict = {}

for gama in range(1, 4):
    normal_dict[str(gama)] = {'logit':[], 'CBPS1':[], 'CBPS2':[]}
    mis_dict[str(gama)] = {'logit':[], 'CBPS1':[], 'CBPS2':[]}

large_normal_dict = {}
large_mis_dict = {}
mid_normal_dict = {}
mid_mis_dict = {}
cur_file = r'E:\LSEPS\CateCode\Non-para的副本\result\2000'
# cur_file = r'E:\LSEPS\CateCode\Non-para的副本\result\500'
os.chdir(cur_file)
out_name = cur_file.split('\\')
out_file_name = 'num=' + out_name[-1] + '.xls'
file_name_list = get_file_name(cur_file)
num = out_name[-1]

for file in file_name_list:
    if file.split('.')[-1] != 'csv':
        continue
    # num = file.split('num=')[1].split('_gama')[0]
    cur_excel = pd.read_csv(os.path.join(cur_file, file), header=0, sep=',')
    data = pd.DataFrame(cur_excel)
    gama = file.split('gama = ')[1].split('alpha0')[0]
    score = data[['X1', 'X2', 'X3', 'X4', 'X5']]
    data['ave'] = data.mean(axis=1)

    for index, ave_num in enumerate(data['ave']):
        if index > 8:
            mis_dict[gama][index_dict[index]].append(round(ave_num, 4))
        else:
            normal_dict[gama][index_dict[index]].append(round(ave_num, 4))
    '''
    if num == '2000':
        large_normal_dict[gama] = (data[4:5].values.tolist()[0][1:])
        large_mis_dict[gama] = data[8:9].values.tolist()[0][1:]
    elif num == '1000':
        mid_normal_dict[gama] = data[4:5].values.tolist()[0][1:]
        mid_mis_dict[gama] = data[8:9].values.tolist()[0][1:]
    else:
        normal_dict[gama] = data[4:5].values.tolist()[0][1:]
        mis_dict[gama] = data[8:9].values.tolist()[0][1:]'
    '''

# out
out_excel = xlwt.Workbook()
ans_sheet = out_excel.add_sheet(sheetname='sheet', cell_overwrite_ok=True)
row = 2
ans_sheet.write(0, 2, out_file_name)
ans_sheet.write(0, 0, 'sample='+num)
ans_sheet.write_merge(1, 1, 2, 4, 'Correct')
ans_sheet.write_merge(1, 1, 5, 7, 'Incorrect')
ans_sheet.write(2, 2, 'Bias')
ans_sheet.write(2, 5, 'Bias')
ans_sheet.write(2, 3, 'StandError')
ans_sheet.write(2, 4, 'RMSE')
#ans_sheet.write(2, 4, 'Overlap_CBPS')
ans_sheet.write(2, 6, 'StandError')
ans_sheet.write(2, 7, 'RMSE')
#ans_sheet.write(2, 10, 'Overlap_CBPS')
"""
ans_sheet.write(7, 0, 'sample = 1000')
ans_sheet.write(8, 0, 'correct')
ans_sheet.write(8, 6, 'incorrect')
ans_sheet.write(9, 1, 'Normal')
ans_sheet.write(9, 7, 'Normal')
ans_sheet.write(9, 2, 'Overlap')
ans_sheet.write(9, 3, 'CBPS')
ans_sheet.write(9, 4, 'Overlap_CBPS')
ans_sheet.write(9, 8, 'Overlap')
ans_sheet.write(9, 9, 'CBPS')
ans_sheet.write(9, 10, 'Overlap_CBPS')


ans_sheet.write(14, 0, 'sample = 2000')
ans_sheet.write(15, 0, 'correct')
ans_sheet.write(15, 6, 'incorrect')
ans_sheet.write(16, 1, 'Normal')
ans_sheet.write(16, 7, 'Normal')
ans_sheet.write(16, 2, 'Overlap')
ans_sheet.write(16, 3, 'CBPS')
ans_sheet.write(16, 4, 'Overlap_CBPS')
ans_sheet.write(16, 8, 'Overlap')
ans_sheet.write(16, 9, 'CBPS')
ans_sheet.write(16, 10, 'Overlap_CBPS')
"""

for row in range(3, 12):
    out_str = index_dict[row]
    gama = row // 3
    if row % 3 == 0:
        ans_sheet.write_merge(row, row+2, 0, 0, 'γ=' + str((row // 3)))
    ans_sheet.write(row, 1, out_str)
    for index in range(3):
        ans_sheet.write(row, index+2, normal_dict[str(gama)][out_str][index])
        ans_sheet.write(row, index+5, mis_dict[str(gama)][out_str][index])

"""
for gama in range(1, 4):

    row = gama * 3
    for index in range(3):
        ans_sheet.write(row, index + 1, large_normal_dict[str(gama)][index])
        ans_sheet.write(row, index + 4, large_mis_dict[str(gama)][index])

    row = gama + 9
    ans_sheet.write(row, 0, gama)
    ans_sheet.write(row, 6, gama)
    for index in range(4):
        ans_sheet.write(row, index+1, mid_normal_dict[str(gama)][index])
        ans_sheet.write(row, index+7, mid_mis_dict[str(gama)][index])

    row = gama + 2
    ans_sheet.write(row, 0, gama)
    ans_sheet.write(row, 6, gama)
    for index in range(4):
        ans_sheet.write(row, index + 1, normal_dict[str(gama)][index])
        ans_sheet.write(row, index + 7, mis_dict[str(gama)][index])
"""

print('successfully load and out, sava file: ', out_file_name)
out_excel.save(os.path.join(r'../', out_file_name))