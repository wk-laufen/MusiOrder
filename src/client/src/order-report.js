import { Workbook } from 'exceljs'

function downloadExcel(buffer, fileName) {
    var blob = new Blob([buffer], { type: 'application/vnd.ms-excel' })
    var link = document.createElement('a')
    link.href = window.URL.createObjectURL(blob)
    link.download = fileName
    link.click()
}

export async function createAndDownloadReport(groupedData, fileName) {
    const workbook = new Workbook()
    const worksheet = workbook.addWorksheet("Bestellungen")
    worksheet.addTable({
        name: 'Bestellungen',
        ref: 'A1',
        headerRow: true,
        totalsRow: true,
        style: {
            theme: 'TableStyleLight1',
            showRowStripes: true,
        },
        columns: [
            { name: 'Name', totalsRowLabel: 'Gesamt', filterButton: true },
            { name: 'Anzahl', totalsRowFunction: 'sum', filterButton: true },
            { name: 'Umsatz', totalsRowFunction: 'sum', filterButton: true },
        ],
        rows: groupedData.map(v => [ v.Name, v.Amount, v.Revenue.toNumber() ]),
    })
    worksheet.getColumn('C').numFmt = '"€"#,##0.00'
    const buffer = await workbook.xlsx.writeBuffer()
    downloadExcel(buffer, fileName)
}
