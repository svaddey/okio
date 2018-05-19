package com.braviithi.okio.controller;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Controller;
import org.springframework.ui.Model;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.ModelAttribute;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.servlet.mvc.support.RedirectAttributes;

import com.braviithi.okio.dto.ReportDto;
import com.braviithi.okio.service.IReportService;

@Controller
public class ReportController {
	@Autowired
	IReportService reportService;
	
	@GetMapping(value = "/")
	public String index(Model model) {
		model.addAttribute("reportDto", new ReportDto());
		return "upload";
	}

	@PostMapping("/upload")
	public String singleFileUpload(@ModelAttribute ReportDto reportDto, RedirectAttributes redirectAttributes) {
		reportService.uploadFile(reportDto);
		redirectAttributes.addFlashAttribute("message",
				"You successfully uploaded '" + reportDto.getFile().getOriginalFilename() + "'");
		return "upload";
//		return "redirect:/uploadStatus";
	}

	@GetMapping("/uploadStatus")
	public String uploadStatus() {
		return "uploadStatus";
	}
}
